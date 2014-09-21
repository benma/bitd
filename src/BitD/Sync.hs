{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module BitD.Sync
       ( daemon
       )
       where

#include <Imports.hs>
import qualified BitD.MemPool as MP
import BitD.Networks (Network(..))
import BitD.Protocol.VarData
import BitD.Util.Daemon (async)
import BitD.Util.Binary (runGet)
import qualified BitD.Util.AMQP as Q
import qualified BitD.DB as DB
import BitD.BitcoinD.BlockStream (produceBlocks)
import qualified BitD.Util.LevelDB as LDB
import qualified Database.LevelDB as LDB'
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Lock as Lock
import BitD.WebService (webservice)
                         
import BitD.Protocol.Types (Address(..), SatoshiValue(..), Hash256(..), Hash256Hex(..), Hash160(..), ToBS(..), TxID(..), toHex)

import BitD.Protocol.BitcoinAddress (addressToHash160)
import qualified BitD.ConnectionDaemon.AMQP as DM

import BitD.Types (ByteOffset)
import qualified BitD.BitcoinD.RPC as RPC
import BitD.BitcoinD.BlockStream (rawBlockStream)
import qualified BitD.Protocol.Blockchain as BC
import Data.Default (def)


import Control.Monad.Trans.Resource (ResourceT)



makeLenses ''BC.Block
makeLenses ''BC.BlockData
makeLenses ''BC.Transaction
makeLenses ''BC.Output
makeLenses ''BC.Input

hexHash :: Getter Hash256 Hash256Hex
hexHash = to toHex


isOnMainChain :: DB.DBBlockchainIndex -> BC.Block -> ResourceT IO (Maybe DB.Height)
isOnMainChain db block = do
  height <- LDB.get db (DB.BlkKeyBlkToHeight $ block ^. prevHash)
  case height of
    Nothing -> return Nothing
    Just (DB.Height height') -> do
      -- todo errs
      Just hash <- liftIO $ RPC.rpcGetBlockHash (DB._dbNetwork db) (height' + 1)
      if hash == block ^. blockHash . hexHash
        then return $ Just $ DB.Height (height' + 1)
        else return Nothing

data BlockEvent = BlockProcessed BC.Block DB.Height ByteOffset ByteOffset | BlockOrphaned Hash256 DB.Height


orphanizeFrom :: DB.DBBlockchainIndex -> DB.Height -> P.Producer' BlockEvent (ResourceT IO) ()
orphanizeFrom db height = P.for (DB.iterBlocks db (Just height) Nothing) $ \(DB.BlkKeyHeightToBlk keyHeight, blkHash) -> do
  -- todo: do batch delete, incoorporate with batch from sync
  log' $ BS.concat ["orphanizing block #", BSC.pack $ show keyHeight, " - ", toBS (blkHash ^. hexHash)]
  P.yield $ BlockOrphaned blkHash keyHeight

  
buildChain :: DB.DBBlockchainIndex -> (ByteOffset, ByteOffset, BC.Block) -> P.Producer BlockEvent (ResourceT IO) ()
buildChain db (offsetBefore, offsetAfter, block) = do
  let curHash = block ^. blockHash 
  let liftDB' = lift 
  let storeTip curHeight = do
        P.yield $ BlockProcessed block curHeight offsetBefore offsetAfter
  
  tip <- liftDB' $ DB.getTip db
  
  case tip of
    Nothing -> storeTip (DB.Height 0)
    Just (DB.Height tipHeight, tip') -> do
      
      let curHeight = DB.Height (tipHeight + 1)
      -- log' $ BS.concat ["handle block #", (BSC.pack $ show curHeight), " - ", toBS curHash]
          
      case (tip' /= block ^. prevHash) of
        False -> storeTip curHeight
        True -> do
          log' ("Current block does not connect to tip!" :: String)
          log' ("Prev: " `BS.append` toBS (block ^. prevHash . hexHash))
          log' ("Cur: " `BS.append` toBS curHash)
          log' ("Tip: " `BS.append` toBS tip')
          log' ("Tip height: " ++ (show tipHeight))
  
          -- TODO errs
          isMainHeight <- liftDB' (isOnMainChain db block)
          case isMainHeight of
            Just height -> do
              log' ("orphanize from " `BS.append` (BSC.pack $ show height) `BS.append` " because of new main chain branch: " `BS.append` toBS curHash)
              orphanizeFrom db height
              storeTip height
            Nothing -> log' ("skipping over orphan " `BS.append` toBS (curHash ^. hexHash))

handleBlockEvent :: MP.MemPool -> DB.DBBlockchainIndex -> BlockEvent -> P.Effect' (ResourceT IO) ()
handleBlockEvent _ db (BlockOrphaned h height) = do
  log' $ BS.concat ["process orphan: ", toBS (h ^. hexHash)]
  
  block <- lift $ DB.getBlockFromHash db h
  
  case block of
    Nothing -> error "couldn't find block"
    Just block' -> do lift $ LDB.commitBatchWriterT db $ do
                        unIndexBlock block' height
                        unIndexBlockTxs db (BC.getBlockData (DB._dbNetwork db) block')
                      log' ("Successfully orphaned block " `BS.append` toBS (h ^. hexHash)) 
  
  
handleBlockEvent memPool db (BlockProcessed block height offsetBefore offsetAfter) = do
  liftIO $ MP.acquireWrite memPool
  
  let blockData = BC.getBlockData (DB._dbNetwork db) block
  let DB.Height height' = height
  M.when (True || height' `rem` 100 == 0) $ log' ("Handling block " ++ show height ++ "; " ++ (show $ blockData ^. txCount) ++ " transactions")
  lift $ LDB.commitBatchWriterT db $ do
    indexBlock block height offsetBefore offsetAfter
    indexBlockTxs db blockData height

  -- remove newly confirmed tx's from mempool
  --M.forM_ (blockData ^. transactions) (liftIO . unindexMempoolTx memPool)
    
  liftIO $ MP.releaseWrite memPool

indexBlock :: BC.Block -> DB.Height -> ByteOffset -> ByteOffset -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
indexBlock block height offsetBefore offsetAfter = do
  let curHash = block ^. blockHash
  LDB.batchWrite (DB.BlkKeyBlkByteOffset curHash) (DB.DBOffset offsetBefore)
  LDB.batchWrite (DB.BlkKeyHeightToBlk height) curHash
  LDB.batchWrite (DB.BlkKeyBlkToHeight curHash) height
  LDB.batchWrite DB.BlkKeyTip curHash
  LDB.batchWrite DB.BlkKeyProcessedOffset (DB.DBOffset offsetAfter)

unIndexBlock :: BC.Block -> DB.Height -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
unIndexBlock block height = do
  let curHash = block ^. blockHash
  LDB.batchDelete (DB.BlkKeyBlkByteOffset curHash)
  LDB.batchDelete (DB.BlkKeyHeightToBlk height)
  LDB.batchDelete (DB.BlkKeyBlkToHeight curHash)

indexBlockTxs :: DB.DBBlockchainIndex -> BC.BlockData -> DB.Height -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
indexBlockTxs db blockData height = do
  iforM_ (blockData ^. transactions) $ \txIdx tx -> do
    LDB.batchWrite (DB.TxKeyBlkRef (tx ^. txHash)) (DB.TxRef height (fromIntegral txIdx))
    indexTx db tx

indexTx :: DB.DBBlockchainIndex -> BC.Transaction -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
indexTx db tx = do
  F.forM_ (tx ^. inputs) $ \(inputIdx, input) -> do
    let prevOutputRef = DB.OutputRef (input ^. prevTxHash) (fromIntegral $ input ^. prevOutputIndex)
    let inputRef = DB.InputRef (tx ^. txHash) inputIdx
    prevDst <- getPreviousDestination input
    case prevDst of
      Nothing -> return ()
      Just destinationHash -> do
        LDB.batchModify db (DB.TxKeyDestinationToOutput destinationHash prevOutputRef) $ \(DB.DBDestination val _) -> DB.DBDestination val (Just inputRef)
        LDB.batchDelete (DB.TxKeyUnspentOutput prevOutputRef)
    
    LDB.batchWrite (DB.TxKeySpent prevOutputRef) (DB.DBInputRef $ DB.InputRef (tx ^. txHash) inputIdx)
  F.forM_ (tx ^. outputs) $ \(outputIdx, output) -> do
    case BC.outputScriptToDestinationHash $ output ^. outputScript of
      Nothing -> return ()
      Just destinationHash -> do
        let outputRef = DB.OutputRef (tx ^. txHash) outputIdx
        LDB.batchWrite (DB.TxKeyDestinationToOutput destinationHash outputRef) (DB.DBDestination (output ^. value) Nothing)
        LDB.batchWrite (DB.TxKeyUnspentOutput outputRef) destinationHash
  where
    -- Get destination of the input's referenced ouptut.
    -- To avoid costly disk seeks, try to guess it from the input.
    -- If that fails, find it by looking it up in the database.
    getPreviousDestination input = do
      case BC.inputScriptToDestinationHash (input ^. inputScript) of
        Just dst -> return $ Just dst 
        Nothing -> do let prevOutputRef = DB.OutputRef (input ^. prevTxHash) (fromIntegral $ input ^. prevOutputIndex)
                      LDB.batchGet db (DB.TxKeyUnspentOutput prevOutputRef)

unIndexBlockTxs :: DB.DBBlockchainIndex -> BC.BlockData -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
unIndexBlockTxs db blockData = do
  iforM_ (blockData ^. transactions) $ \_ tx -> do
    LDB.batchDelete (DB.TxKeyBlkRef (tx ^. txHash))
    unindexTx db tx


unindexTx :: DB.DBBlockchainIndex -> BC.Transaction -> LDB.BatchWriterT DB.DBBlockchainIndex (ResourceT IO) ()
unindexTx db tx = do
  F.forM_ (tx ^. inputs) $ \(_, input) -> do
    let prevOutputRef = DB.OutputRef (input ^. prevTxHash) (fromIntegral $ input ^. prevOutputIndex)
    prevDst <- getPreviousDestination input
    case prevDst of
      Nothing -> return ()
      Just destinationHash -> do
        LDB.batchModify db (DB.TxKeyDestinationToOutput destinationHash prevOutputRef) $ \(DB.DBDestination val _) -> DB.DBDestination val Nothing
        LDB.batchWrite (DB.TxKeyUnspentOutput prevOutputRef) destinationHash
    
    LDB.batchDelete (DB.TxKeySpent prevOutputRef)
      
  F.forM_ (tx ^. outputs) $ \(outputIdx, output) -> do
    case BC.outputScriptToDestinationHash $ output ^. outputScript of
      Nothing -> return ()
      Just destinationHash -> do
        let outputRef = (DB.OutputRef (tx ^. txHash) outputIdx)
        LDB.batchDelete (DB.TxKeyDestinationToOutput destinationHash outputRef)
        LDB.batchDelete (DB.TxKeyUnspentOutput outputRef)
  where
    -- Get destination of the input's referenced ouptut.
    -- To avoid costly disk seeks, try to guess it from the input.
    -- If that fails, find it by looking it up in the database.
    getPreviousDestination input = do
      case BC.inputScriptToDestinationHash (input ^. inputScript) of
        Just dst -> return $ Just dst 
        Nothing -> do let prevOutputRef = DB.OutputRef (input ^. prevTxHash) (fromIntegral $ input ^. prevOutputIndex)
                      LDB.batchGet db (DB.TxKeyUnspentOutput prevOutputRef)

    
newTxQueueName :: T.Text
newTxQueueName = "bitd.sync.inv"

data Event = EventSync
           | EventDaemonMessage (IO (), DM.Message)
           | EventNewTxs [BS.ByteString]

sync :: MP.MemPool -> DB.DBBlockchainIndex -> ResourceT IO ()
sync memPool db = do
  lift $ print ("try" :: String)
  lift $ print ("kick off sync" :: String)
  let consumeBlock block = do
        buildChain db block P.>-> P.for P.cat (handleBlockEvent memPool db)
  
  DB.DBOffset processedOffset <- LDB.getDefault db (DB.DBOffset 0) DB.BlkKeyProcessedOffset

  let network = DB._dbNetwork db
  P.runEffect $ P.for (produceBlocks network processedOffset (rawBlockStream network)) consumeBlock
  
  lift $ print ("finish sync" :: String)

indexMempoolTx :: MP.MemPool -> DB.DBBlockchainIndex -> BC.Transaction -> ResourceT IO ()
indexMempoolTx memPool db tx = do
  let txHash' = BC._txHash tx
  (lift $ MP.txInMemPool memPool txHash') >>= \case
    True -> log' ("ignoring new tx, already in mem pool" :: String)
    False -> DB.txIsConfirmed db txHash' >>= \case
      True -> do
        log' ("new tx already confirmed; removing from mempool" :: String)
        lift $ unindexMempoolTx memPool tx
      False -> hasConflicts >>= \case
        True -> do
          log' ("new tx is a double spend; removing from mempool" :: String)
          lift $ unindexMempoolTx memPool tx
        False -> lift $ MP.addTransaction memPool tx
  where
    prevOutputs = map (\(_, input) -> DB.OutputRef (input ^. prevTxHash) (input ^. prevOutputIndex)) (tx ^. inputs)
    hasConflicts = any id <$> M.mapM (DB.isSpent memPool db) prevOutputs
      
unindexMempoolTx :: MP.MemPool -> BC.Transaction -> IO ()
unindexMempoolTx memPool tx = do
  MP.removeTransaction memPool tx False
  MP.removeTransactionsConflictingWith memPool tx

initMemPool :: DB.DBBlockchainIndex -> ResourceT IO MP.MemPool
initMemPool db = do
  (memPool, restoreTxs) <- MP.initMemPool $ DB._dbNetwork db
  F.forM_ restoreTxs (indexMempoolTx memPool db)
  return memPool

daemon :: Network -> ResourceT IO ()
daemon network = do

  chan <- liftResource $ Q.open
  (txQueue,_,_) <- liftIO $ Q.declareQueuePersistent chan newTxQueueName
  liftIO $ Q.bindQueue chan txQueue "inv"

  liftIO $ print $ B16.encode $ Ser.encode (VarInt 1)
  db <- liftResource $ DB.initDB network

  log' ("initmempool start" :: String)
  memPool <- liftResource $ initMemPool db
  log' ("initmempool end" :: String)

  Just realheight <- liftIO $ RPC.rpcGetBlockCount network
  log' $ "Current tip on node: " ++ show realheight

  blub <- liftIO $ Lock.new
  let syncPrint a = Lock.with blub $ print a

  liftResource $ DB.getTip db >>= (^! _Just . act (\(height, tip') -> lift $ syncPrint $ BS.concat ["Current tip: #", BSC.pack $ show height, " - ", toBS tip']))
  
  (output, input, _) <- liftIO $ PC.spawn' PC.Single
  
  let kickOffSync delay = async $ do
        doAfterDelay delay $ P.runEffect $ P.yield EventSync P.>-> PC.toOutput output
  
  let consumeDaemonMessage (DM.MsgNewTransaction rawTx) = consumeNewTx rawTx
      consumeDaemonMessage (DM.MsgNewBlock hash) = consumeNewBlock hash
      consumeNewBlock hash ack = do
        liftIO $ syncPrint $ "New Block: " `BS.append` (toHex hash)
        kickOffSync Nothing
        liftIO $ ack
      consumeNewTx rawTx ack = do
        case Bin.decodeOrFail (BSL.fromStrict rawTx) of
          Left _ -> do
            liftIO $ syncPrint ("failed to parse new transaction" :: String)
          Right (_,_,tx) -> do
            liftIO $ syncPrint $ "New Tx: " `BS.append` (toHex (BC._txHash tx) :: BS.ByteString)
            indexMempoolTx memPool db tx
              --threadDelay 2000000
        liftIO ack
        
      consume = M.forever $ do
        event <- P.await
        lift $ case event of
          EventNewTxs rawTxs -> F.forM_ rawTxs $ \rawTx -> consumeNewTx rawTx (return ())
          EventSync -> do sync memPool db
                          -- reschedule sync
                          let delay = 1000000 * 30 -- 30s
                          kickOffSync (Just delay)
                            
  
          EventDaemonMessage (ack, msg) -> consumeDaemonMessage msg ack

        
  
  liftResource $ kickOffSync Nothing
    
  liftResource $ async $ Q.consumeMsgs' chan txQueue EventDaemonMessage output

  let printMemPoolInfo = do
        MP.countTransactions memPool >>= syncPrint
        doAfterDelay (Just 100000) printMemPoolInfo

  let syncNodeMemPool = do
        -- todo: error handling, rpc call can return Nothing
        Just txHashes <- RPC.rpcGetRawMemPool' network
        txHashes' <- M.filterM (\txHash' -> not <$> MP.txInMemPool memPool txHash') txHashes
        liftIO $ syncPrint $ show (length txHashes') ++ " new txs from node's mempool"
        P.runEffect $ (F.forM_ txHashes' $ \txHash' -> do
          Just rawTx <- lift $ RPC.rpcGetRawTransaction network txHash'
          P.yield (EventNewTxs [rawTx])) P.>-> PC.toOutput output

  
        -- Just txHashes <- RPC.rpcGetRawMemPool'
        -- mytxHashes <- MP.getTransactions memPool
        -- liftIO $ syncPrint $ Set.fromList mytxHashes `Set.difference` Set.fromList txHashes
        doAfterDelay (Just (1000000)) syncNodeMemPool

        
  -- liftResource $ async printMemPoolInfo
  -- liftResource $ async syncNodeMemPool
  
  liftResource $ async $ webservice memPool db
  liftResource $ P.runEffect $ PC.fromInput input P.>-> consume
  where
    liftResource = id

log' :: (MonadIO m, Show s) => s -> m ()
log' m = liftIO . debugM "sync" . show $ m

doAfterDelay :: (Maybe Int) -> IO a -> IO a
doAfterDelay Nothing m = m
doAfterDelay (Just delay) m = threadDelay delay >> m
