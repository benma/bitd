{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module BitD.MemPool
       ( initMemPool
       , MemPool
       , getTransaction
       , txInMemPool
       , getBalance
       , isSpent
       , removeTransaction
       , removeTransactions
       , removeTransactionsConflictingWith
       , addTransaction
       , addTransactions
       , countTransactions
       , getTransactions
       , acquireWrite
       , releaseWrite
       , withWrite
       )
       where

#include <Imports.hs>

import BitD.Networks (Network(..))
import qualified Data.Acid as Acid
import qualified BitD.Protocol.Blockchain as BC
import BitD.Protocol.Types (TxID(..), Hash160(..), Hash256(..), SatoshiValue(..))
import BitD.DB.Types (InputRef(..), OutputRef(..))
import BitD.MemPool.HashMap (HashMap(..))
import qualified BitD.MemPool.Acid as Acid
import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Control.Concurrent.ReadWriteLock as RWLock

                             

data MemPool' = MemPool' { _transactions :: !(Map.HashMap TxID BC.RawTransaction)
                         , _destinations :: !(Map.HashMap Hash160 (Map.HashMap OutputRef (SatoshiValue, Maybe InputRef)))
                         , _spentOutputs :: !(Map.HashMap OutputRef InputRef)
                       }


type MemPool = (RWLock.RWLock, IORef.IORef MemPool', Acid.AcidState Acid.AcidMemPool)

newMemPool :: MemPool'
newMemPool = MemPool' { _transactions = Map.empty
                      , _destinations = Map.empty
                      , _spentOutputs = Map.empty
                      }

makeLenses ''MemPool'

query :: MemPool -> (MemPool' -> a) -> IO a
query (lock,mp,_) q = RWLock.withRead lock $ (q <$> IORef.readIORef mp)

update :: MemPool -> State.StateT MemPool' IO () -> IO ()
update (_,mp,_) f = do
  -- IORef.modifyIORef' mp (State.execState f)
  x <- IORef.readIORef mp
  x' <- State.execStateT f x
  x' `seq` IORef.writeIORef mp x'

getTransaction :: MemPool -> TxID -> IO (Maybe BC.RawTransaction)
getTransaction mp txHash = query mp $ \mp' -> mp' ^. transactions . at txHash

getTransactions :: MemPool -> IO [TxID]
getTransactions mp = query mp $ \mp' -> Map.keys $ mp' ^. transactions

txInMemPool :: MemPool -> TxID -> IO Bool
txInMemPool mp txHash = isJust <$> getTransaction mp txHash


getBalance :: MemPool -> Hash160 -> IO SatoshiValue
getBalance mp dst = query mp $ \mp' ->
  case mp' ^. destinations . at dst of
    Nothing -> 0
    Just outputs -> F.foldl' (+) 0 $ map (\(_, (val,_)) -> val) $ filter (isSpent' mp' . fst) $ Map.toList outputs

isSpent' :: MemPool' -> OutputRef -> Bool
isSpent' mp' outputRef = case mp' ^. spentOutputs . at outputRef of
  Nothing -> True
  Just _ -> False

isSpent :: MemPool -> OutputRef -> IO Bool
isSpent mp outputRef = query mp $ \mp' -> case mp' ^. spentOutputs . at outputRef of
  Nothing -> False
  Just _ -> True

addTransaction :: MemPool -> BC.Transaction -> IO ()
addTransaction mp@(_,_,acid) tx = do
  Acid.addTransaction acid tx
  update mp $ do
    transactions . at (BC._txHash tx) ?= (BC.RawTransaction $ BSL.toStrict $ Bin.encode tx)
        
    F.forM_ (BC._inputs tx) $ \(inputIdx, input) -> do
      let prevOutputRef = OutputRef (BC._prevTxHash input) (fromIntegral $ BC._prevOutputIndex input)
      let inputRef = InputRef (BC._txHash tx) inputIdx
  
      spentOutputs . at prevOutputRef ?= inputRef
      
    F.forM_ (BC._outputs tx) $ \(outputIdx, output) -> do
      case BC.outputScriptToDestinationHash (BC._outputScript output) of
        Nothing -> return ()
        Just destinationHash -> do
          let outputRef = OutputRef (BC._txHash tx) outputIdx
          --destinations . at destinationHash . at outputRef) ?= (BC._value output, Nothing)
          destinations . at destinationHash %= \case
            Nothing -> Just $ Map.singleton outputRef (BC._value output, Nothing)
            Just m -> Just $ Map.insert outputRef (BC._value output, Nothing) m

addTransactions :: MemPool -> [BC.Transaction] -> IO ()
addTransactions mp = F.mapM_ (addTransaction mp)

removeTransaction :: MemPool -> BC.Transaction -> Bool -> IO ()
removeTransaction mp@(_,_,acid) tx recursive = do
  update mp $ do
    transactions . at (BC._txHash tx) .= Nothing
    
    F.forM_ (BC._inputs tx) $ \(_, input) -> do
      let prevOutputRef = OutputRef (BC._prevTxHash input) (fromIntegral $ BC._prevOutputIndex input)
      spentOutputs . at prevOutputRef .= Nothing
      
    F.forM_ (BC._outputs tx) $ \(outputIdx, output) -> do
      let outputRef = OutputRef (BC._txHash tx) outputIdx
  
      case BC.outputScriptToDestinationHash (BC._outputScript output) of
        Nothing -> return ()
        Just destinationHash -> do
          destinations . at destinationHash . _Just . at outputRef .= Nothing
  
      -- recursively remove tx's that depend on this output
      M.when recursive $ do
        use (spentOutputs . at outputRef) >>= \case
          Nothing -> return ()
          Just (InputRef txId _) -> do
            use (transactions . at txId) >>= \case
              Nothing -> return ()
              Just rawTx -> liftIO $ removeTransaction mp (rawTx ^. BC.txIso) recursive

  Acid.removeTransaction acid tx
  
removeTransactions :: MemPool -> [BC.Transaction] -> Bool -> IO ()
removeTransactions mp txs recursive = F.mapM_ (\tx -> removeTransaction mp tx recursive) txs

removeTransactionsConflictingWith :: MemPool -> BC.Transaction -> IO ()
removeTransactionsConflictingWith mp tx = update mp $ do
  F.forM_ (BC._inputs tx) $ \(inputIdx, input) -> do
    let prevOutputRef = OutputRef (BC._prevTxHash input) (fromIntegral $ BC._prevOutputIndex input)
    let inputRef = InputRef (BC._txHash tx) inputIdx
        
    use (spentOutputs . at prevOutputRef) >>= \case
      Nothing -> return ()
      Just inputRef'@(InputRef txId _) -> M.when (inputRef' /= inputRef) $ do
        use (transactions . at txId) >>= \case
          Nothing -> return ()
          Just rawTx -> liftIO $ removeTransaction mp (rawTx ^. BC.txIso) True
        
countTransactions :: MemPool -> IO Int
countTransactions mp = query mp $ \mp' -> Map.size $ mp' ^. transactions

acquireWrite :: MemPool -> IO ()
acquireWrite (lock,_,_) = RWLock.acquireWrite lock

withWrite :: MemPool -> IO () -> IO ()
withWrite (lock,_,_) = RWLock.withWrite lock

releaseWrite :: MemPool -> IO ()
releaseWrite (lock,_,_) = RWLock.releaseWrite lock

initMemPool :: Network -> ResourceT IO (MemPool, [BC.Transaction])
initMemPool network = do
  lock <- lift RWLock.new
  memPool <- lift $ IORef.newIORef newMemPool
  acidMemPool <- Acid.initAcid network

  let mp = (lock, memPool, acidMemPool)

  restoreTxs <- lift $ Acid.getTransactions acidMemPool -- >>= F.mapM_ (addTransaction mp)

  return (mp, restoreTxs)

