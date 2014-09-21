{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module BitD.DB
       ( module BitD.DB.LevelDB
       , initDB
       , iterBlocks
       , iterDestinationOutputs
       , getBlockFromHash
       , getTip
       , getBalance
       , isSpent
       , txIsConfirmed
       )
       where

#include <Imports.hs>

import qualified BitD.MemPool as MP
import BitD.Networks (Network(..))
import BitD.DB.LevelDB
import BitD.DB.Types (Balance(..))
import BitD.Protocol.Types (Hash256(..), Hash256Hex(..), Hash160(..), ToBS(..), TxID(..), SatoshiValue(..))
import BitD.Types (ByteOffset)
import System.Directory (createDirectoryIfMissing)
import qualified BitD.Util.LevelDB as LDB
import Control.Monad.Trans.Resource (ResourceT)
import qualified BitD.Protocol.Blockchain as BC
import BitD.BitcoinD.BlockStream (getBlockAtOffset)



dbPath :: Network -> IO.FilePath
dbPath BTCMain = "/home/ums/bitchange/data/btcmain/db"
dbPath BTCTest = "/home/ums/bitchange/data/btctest/db"

initDB :: Network -> ResourceT IO DBBlockchainIndex
initDB network = do
  liftIO $ createDirectoryIfMissing True $ dbPath network

  db <- DBBlockchainIndex network <$> (LDB.open $ dbPath network)
  log' ("opened db" :: String)

  return db
  
log' :: (MonadIO m, Show s) => s -> m ()
log' m = liftIO . debugM "sync" . show $ m
    -- debug = liftIO . debugM "sync" . show

iterBlocks :: DBBlockchainIndex -> Maybe Height -> Maybe Height -> P.Producer' (BlkKeyHeightToBlk, Hash256) (ResourceT IO) ()
iterBlocks db start end = LDB.dbIter db (Just $ BlkKeyHeightToBlk $ maybe minBound id start) (Just $ BlkKeyHeightToBlk $ maybe maxBound id end)

iterDestinationOutputs :: DBBlockchainIndex -> Hash160 -> P.Producer' (TxKeyDestinationToOutput, DBDestination) (ResourceT IO) ()
iterDestinationOutputs db dst = LDB.dbIter db (Just $ TxKeyDestinationToOutput dst minBound) (Just $ TxKeyDestinationToOutput dst maxBound)

getBlockFromHash :: DBBlockchainIndex -> Hash256 -> ResourceT IO (Maybe BC.Block)
getBlockFromHash db hash = do
  LDB.get db (BlkKeyBlkByteOffset hash) >>= \case
    Nothing -> return Nothing
    Just (DBOffset offset) -> lift $ getBlockAtOffset (_dbNetwork db) offset

getTip :: DBBlockchainIndex -> ResourceT IO (Maybe (Height, Hash256))
getTip db = do
  tip <- LDB.get db BlkKeyTip
  case tip of
    Nothing -> return Nothing
    Just tip' -> do
      height <- LDB.getDefault db (Height 0) (BlkKeyBlkToHeight tip')
      return $ Just (height, tip')

getBalance :: MP.MemPool -> DBBlockchainIndex -> Hash160 -> ResourceT IO Balance
getBalance memPool db dst = do
  balance <- PPL.fold mappend mempty id $ P.for (iterDestinationOutputs db dst) $ \(TxKeyDestinationToOutput _ outputRef, DBDestination (SatoshiValue val) spent) -> do
    case spent of
      Nothing -> liftIO (MP.isSpent memPool outputRef) >>= \case
        True -> P.yield $ Balance (SatoshiValue val) (SatoshiValue 0)
        False -> P.yield $ Balance (SatoshiValue val) (SatoshiValue val)
      Just _ -> return ()
  unconfirmed' <- lift $ MP.getBalance memPool dst
  return $ balance <> Balance 0 unconfirmed'

isSpent :: MP.MemPool -> DBBlockchainIndex -> OutputRef -> ResourceT IO Bool
isSpent memPool db outputRef = do
  LDB.get db (TxKeySpent outputRef) >>= \case
    Just _ -> return True
    Nothing -> lift $ MP.isSpent memPool outputRef

txIsConfirmed :: DBBlockchainIndex -> TxID -> ResourceT IO Bool
txIsConfirmed db txHash = isJust <$> LDB.get db (TxKeyBlkRef txHash)
