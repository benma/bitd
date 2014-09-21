{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module BitD.Util.LevelDB ( SerializeValue(..)
                         , SerializeKey(..)
                         , serializeKey'
                         , deserializeKey'
                         , serializeValue'
                         , deserializeValue'
                         , LevelDBKey
                         , DBOp(..)
                         , LDB.DB
                         , LDB.runResourceT
                         , open
                         , LDB(..)
                         , getDefault
                         , get
                         , put
                         , del
                         , write
                         , dbIter, dbIter'
                         , BatchWriterT
                         , runBatchWriterT
                         , commitBatchWriterT
                         , batchWrite
                         , batchGet
                         , batchDelete
                         , batchModify
                         ) where

#include <Imports.hs>

import Control.Monad.Trans.Resource (ResourceT, release)
import qualified Database.LevelDB as LDB
import Data.Default (def)
import BitD.Util.State (modify')

import Data.Hashable (Hashable(..))
import Data.Eq (Eq(..))

class SerializeKey a where
  serializeKey :: SerP.Putter a
  deserializeKey :: SerG.Get a

class SerializeValue a where
  serializeValue :: SerP.Putter a
  deserializeValue :: SerG.Get a

instance SerializeValue Word32 where
  serializeValue a = SerP.putWord32be a
  deserializeValue = SerG.getWord32be

instance SerializeValue Int32 where
  serializeValue = SerP.putWord32be . fromIntegral
  deserializeValue = fromIntegral <$> SerG.getWord32be

instance SerializeValue Word64 where
  serializeValue a = SerP.putWord64be a
  deserializeValue = SerG.getWord64be

instance SerializeValue Int64 where
  serializeValue = SerP.putWord64be . fromIntegral
  deserializeValue = fromIntegral <$> SerG.getWord64be

instance (SerializeValue a, SerializeValue b) => SerializeValue (a, b) where
  serializeValue (a, b) = serializeValue a >> serializeValue b
  deserializeValue = (,) <$> deserializeValue <*> deserializeValue

  
instance (SerializeValue a) => SerializeValue (Maybe a) where
  
  serializeValue Nothing = SerP.putWord8 0
  serializeValue (Just a) = SerP.putWord8 1 >> serializeValue a
  
  deserializeValue = do
    tag <- SerG.getWord8
    case tag of
      0 -> return Nothing
      _ -> Just <$> deserializeValue


instance SerializeKey Word32 where
  serializeKey a = SerP.putWord32be a
  deserializeKey = SerG.getWord32be

instance SerializeKey Int32 where
  serializeKey = SerP.putWord32be . fromIntegral
  deserializeKey = fromIntegral <$> SerG.getWord32be

instance SerializeKey Word64 where
  serializeKey a = SerP.putWord64be a
  deserializeKey = SerG.getWord64be

instance SerializeKey Int64 where
  serializeKey = SerP.putWord64be . fromIntegral
  deserializeKey = fromIntegral <$> SerG.getWord64be


class LDB a where
  getDb :: a -> LDB.DB

deserializeKey' :: SerializeKey a => BS.ByteString -> a
deserializeKey' = either error id . SerG.runGet deserializeKey

serializeKey' :: SerializeKey a => a -> BS.ByteString
serializeKey' k = SerP.runPut (serializeKey k)

deserializeValue' :: SerializeValue a => BS.ByteString -> a
deserializeValue' = either error id . SerG.runGet deserializeValue

serializeValue' :: SerializeValue a => a -> BS.ByteString
serializeValue' v = SerP.runPut (serializeValue v)


class (LDB db, SerializeKey key, SerializeValue val) => LevelDBKey db key val | key -> val, key -> db

data DBOp db where
  DBPut :: LevelDBKey db key val => key -> val -> DBOp db
  DBDel :: LevelDBKey db key val => key -> DBOp db
      
open :: LDB.MonadResource m => IO.FilePath -> m LDB.DB
open filepath = do
  -- bloom <- LDB.bloomFilter 10
  LDB.open filepath def { LDB.createIfMissing = True
                        --, LDB.filterPolicy = Just . Left $ bloom
                        }

getDefault :: LevelDBKey db key val => db -> val -> key -> ResourceT IO val
getDefault db v key = maybe v id <$> get db key
get :: LevelDBKey db key val => db -> key -> ResourceT IO (Maybe val)
get db key = liftA deserializeValue' <$> LDB.get (getDb db) def (serializeKey' key)
put :: LevelDBKey db key val => db -> key -> val -> ResourceT IO ()
put db key val = LDB.put (getDb db) def (serializeKey' key) (serializeValue' val)
del :: LevelDBKey db key val => db -> key -> ResourceT IO ()
del db key = LDB.delete (getDb db) def (serializeKey' key)
write :: LDB db => db -> [DBOp db] -> ResourceT IO ()
write db xs = LDB.write (getDb db) def $ map go xs
  where go (DBPut k v) = LDB.Put (serializeKey' k) (SerP.runPut $ serializeValue v)
        go (DBDel k) = LDB.Del (serializeKey' k)


dbIter :: (LevelDBKey db key val) => db -> Maybe key -> Maybe key -> P.Producer' (key, val) (ResourceT IO) ()
dbIter db start end = dbIter' db (serializeKey' <$> start) (serializeKey' <$> end)
                      P.>->
                      PPL.map (\(key, val) -> (deserializeKey' key, deserializeValue' val))
  
dbIter' :: (LDB db) => db -> Maybe BS.ByteString -> Maybe BS.ByteString -> P.Producer' (BS.ByteString, BS.ByteString) (ResourceT IO) ()
dbIter' db start end = do
  (rk, iter) <- lift $ LDB.iterOpen' (getDb db) def
  lift $ maybe (LDB.iterFirst iter) (LDB.iterSeek iter) start
  pull iter
  release rk
  where
    pull iter = do
      valid <- lift $ LDB.iterValid iter
      M.when valid $ do
        key <- maybe (error "impossible") id <$> (lift $ LDB.iterKey iter)
        val <- maybe (error "impossible") id <$> (lift $ LDB.iterValue iter)
        
        let continue = case end of
              Just end' -> key <= end'
              Nothing -> True

        M.when continue $ do
          lift $ LDB.iterNext iter
          P.yield (key, val)
          pull iter

data BatchWriterState db = BatchWriterState (D.DList (DBOp db)) (Map.HashMap BS.ByteString (Maybe BS.ByteString))

newtype BatchWriterT db m r = BatchWriterT (State.StateT (BatchWriterState db) m r)
                        deriving (Monad, MonadTrans, Functor, Applicative)

runBatchWriterT :: (Monad m) => BatchWriterT db m () -> m [DBOp db]
runBatchWriterT (BatchWriterT m) = do
  BatchWriterState l _ <- State.execStateT m (BatchWriterState D.empty Map.empty)
  return $ D.toList l

commitBatchWriterT :: (LDB db) => db -> BatchWriterT db (ResourceT IO) () -> ResourceT IO ()
commitBatchWriterT db m = runBatchWriterT m >>= write db

batchWrite :: (LevelDBKey db k v, Monad m) => k -> v -> BatchWriterT db m ()
batchWrite k v = BatchWriterT $ modify' (\(BatchWriterState l ops) -> BatchWriterState
                                                                     (l `D.snoc` (DBPut k v))
                                                                     (Map.insert (serializeKey' k) (Just (serializeValue' v)) ops)
                                       )

batchDelete :: (LevelDBKey db k v, Monad m) => k -> BatchWriterT db m ()
batchDelete k = BatchWriterT $ modify' (\(BatchWriterState l ops) -> BatchWriterState
                                                                    (l `D.snoc` (DBDel k))
                                                                    (Map.insert (serializeKey' k) Nothing ops)
                                      )
batchGet :: (LevelDBKey db k v) => db -> k -> BatchWriterT db (ResourceT IO) (Maybe v)
batchGet db k = BatchWriterT $ do
  BatchWriterState _ ops <- State.get
  -- look up in current batch
  case Map.lookup (serializeKey' k) ops of
    Just el -> return $ deserializeValue' <$> el
    -- fall back to leveldb if not found
    Nothing -> lift (get db k)
  
-- batchModify db k f = do
--   lift (get db k) >>= \case
--     Nothing -> return ()
--     Just old -> batchWrite k (f old)

batchModify :: (LevelDBKey db k v) => db -> k -> (v -> v) -> BatchWriterT db (ResourceT IO) ()
batchModify db k f = do
  (batchGet db k) >>= \case
    Nothing -> return ()
    Just old -> batchWrite k (f old)
