{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module BitD.DB.LevelDB
       ( DBBlockchainIndex(..)
       , BlkKeyProcessedOffset(..)
       , BlkKeyTip(..)
       , BlkKeyHeightToBlk(..)
       , BlkKeyBlkToHeight(..)
       , BlkKeyBlkByteOffset(..)
         
       , TxKeyBlkRef(..)
       , TxKeyDestinationToOutput(..)
       , TxKeyUnspentOutput(..)
       , TxKeySpent(..)
         
       --, TxKeyMemPoolKey(..)
       -- , TxKeyMemPoolMember(..)
       -- , MemPoolTx(..)
       -- , RawTransaction(..)
       -- , MemPoolTxStatus(..)
         
       , OutputRef(..), InputRef(..)
       , Height(..), DBOffset(..), DBInputRef(..), TxRef(..), DBSatoshiValue(..), DBDestination(..)

       ) where

#include <Imports.hs>

import BitD.Networks (Network)
import BitD.DB.Types (OutputRef(..), InputRef(..))
import qualified BitD.Util.LevelDB as LDB
import BitD.Protocol.Types (Hash256(..), Hash256Hex(..), Hash160(..), ToBS(..), TxID(..), SatoshiValue(..))
import BitD.Types (ByteOffset)
import qualified BitD.Util.Serialize as SerUtil
import qualified BitD.Util.Binary as BinUtil
import qualified Data.Aeson as JSON

data DBBlockchainIndex = DBBlockchainIndex { _dbNetwork :: Network
                                           , _db :: LDB.DB
                                           } 

instance LDB.LDB DBBlockchainIndex where
  getDb (DBBlockchainIndex _ db') = db'

newtype Height = Height Word32
                 deriving (LDB.SerializeKey, LDB.SerializeValue, Bounded, JSON.ToJSON)

instance Show Height where
  show (Height h) = show h

data TxRef = TxRef Height Word32

data Version = Version000
             | Version001

instance Ser.Serialize Version where
  put Version000 = SerP.putWord8 0
  put Version001 = SerP.putWord8 1

  get = SerG.getWord8 >>= \case
    0 -> return Version000
    1 -> return Version001
    _ -> error "unrecognized version"

instance LDB.SerializeValue TxRef where
  
  serializeValue (TxRef height idx) = LDB.serializeValue (height, idx)
  
  deserializeValue = do (height, idx) <- LDB.deserializeValue
                        return $ TxRef height idx
                   

newtype DBSatoshiValue = DBSatoshiValue SatoshiValue

instance LDB.SerializeValue DBSatoshiValue where
  
  serializeValue (DBSatoshiValue (SatoshiValue value)) = LDB.serializeValue value
  
  deserializeValue = (DBSatoshiValue . SatoshiValue) <$> LDB.deserializeValue


-- data MemPoolTxStatus = TxUnknown
--                      | TxNormal

-- instance LDB.SerializeValue MemPoolTxStatus where
--   serializeValue TxUnknown = SerP.putWord8 0
--   serializeValue TxNormal = SerP.putWord8 1

--   deserializeValue = SerG.getWord8 >>= \case
--     0 -> return TxUnknown
--     1 -> return TxNormal

-- data RawTransaction = RawTransaction !BS.ByteString
-- instance LDB.SerializeValue RawTransaction where
--   serializeValue (RawTransaction raw) = Ser.put raw
--   deserializeValue = RawTransaction <$> Ser.get
  
-- data MemPoolTx = MemPoolTx MemPoolTxStatus !RawTransaction

-- instance LDB.SerializeValue MemPoolTx where
--   serializeValue (MemPoolTx status rawTx) = LDB.serializeValue status >> LDB.serializeValue rawTx
--   deserializeValue = MemPoolTx <$> LDB.deserializeValue <*> LDB.deserializeValue

-- value, spent at input
data DBDestination = DBDestination SatoshiValue (Maybe InputRef)
                 
instance LDB.SerializeValue DBDestination where
  
  serializeValue (DBDestination value inputRef) = do
    LDB.serializeValue (DBSatoshiValue value)
    LDB.serializeValue (DBInputRef <$> inputRef)
    
  deserializeValue = do
    DBSatoshiValue value <- LDB.deserializeValue
    inputRef <- LDB.deserializeValue
    let inputRef' = case inputRef of
          Nothing -> Nothing
          Just (DBInputRef ref) -> Just ref
    return $ DBDestination value inputRef'

newtype DBOffset = DBOffset ByteOffset

instance LDB.SerializeValue DBOffset where
  
  serializeValue (DBOffset offset) = LDB.serializeValue offset
  
  deserializeValue = DBOffset <$> LDB.deserializeValue

newtype DBInputRef = DBInputRef InputRef

instance LDB.SerializeValue DBInputRef where
  
  serializeValue (DBInputRef (InputRef txID inputIdx)) = LDB.serializeValue txID >> LDB.serializeValue inputIdx
  
  deserializeValue = do txID <- LDB.deserializeValue
                        inputIdx <- LDB.deserializeValue
                        return $ DBInputRef $ InputRef txID inputIdx

data BlkKeyProcessedOffset = BlkKeyProcessedOffset
data BlkKeyTip = BlkKeyTip
data BlkKeyHeightToBlk = BlkKeyHeightToBlk Height
data BlkKeyBlkToHeight = BlkKeyBlkToHeight Hash256
data BlkKeyBlkByteOffset = BlkKeyBlkByteOffset Hash256


data TxKeyBlkRef = TxKeyBlkRef TxID
  
data TxKeyDestinationToOutput = TxKeyDestinationToOutput Hash160 OutputRef

-- data TxKeyMemPoolKey key = TxKeyMemPoolKey key

-- instance (LDB.SerializeKey key) => LDB.SerializeKey (TxKeyMemPoolKey key) where
--   serializeKey (TxKeyMemPoolKey key) = SerP.putByteString "mem-" >> LDB.serializeKey key
--   deserializeKey = do
--     SerUtil.matchBS "mem-"
--     TxKeyMemPoolKey <$> LDB.deserializeKey

data TxKeyMemPoolMember = TxKeyMemPoolMember TxID --MemPoolTxStatus

instance LDB.SerializeKey TxKeyMemPoolMember where
  serializeKey (TxKeyMemPoolMember txId) = do SerP.putByteString "mem-txm-"
                                              LDB.serializeKey txId

  deserializeKey = do SerUtil.matchBS "mem-txm-"
                      TxKeyMemPoolMember <$> LDB.deserializeKey


data TxKeyUnspentOutput = TxKeyUnspentOutput OutputRef
  
data TxKeySpent = TxKeySpent OutputRef

instance LDB.SerializeKey BlkKeyProcessedOffset where
  serializeKey BlkKeyProcessedOffset = SerP.putByteString "processed-offset"
  deserializeKey = SerUtil.matchBS "processed-offset" *> pure BlkKeyProcessedOffset

instance LDB.SerializeKey BlkKeyTip where
  
  serializeKey BlkKeyTip = SerP.putByteString "tip"
  
  deserializeKey = SerUtil.matchBS "tip" *> pure BlkKeyTip

instance LDB.SerializeKey BlkKeyHeightToBlk where
  
  serializeKey (BlkKeyHeightToBlk v) = do SerP.putByteString "blk-"
                                          LDB.serializeKey v
                                          
  deserializeKey = do SerUtil.matchBS "blk-"
                      BlkKeyHeightToBlk <$> LDB.deserializeKey

instance LDB.SerializeKey BlkKeyBlkByteOffset where
  
  serializeKey (BlkKeyBlkByteOffset h) = do SerP.putByteString "blo-"
                                            LDB.serializeKey h
                                            
  deserializeKey = do SerUtil.matchBS "blo-"
                      BlkKeyBlkByteOffset <$> LDB.deserializeKey

instance LDB.SerializeKey BlkKeyBlkToHeight where
  
  serializeKey (BlkKeyBlkToHeight h) = do SerP.putByteString "idx-"
                                          LDB.serializeKey h

  deserializeKey = do SerUtil.matchBS "idx-"
                      BlkKeyBlkToHeight <$> LDB.deserializeKey

                      
instance LDB.SerializeKey TxKeyBlkRef where

  serializeKey (TxKeyBlkRef txId) = do SerP.putByteString "txb-"
                                       LDB.serializeKey txId

  deserializeKey = do SerUtil.matchBS "txb-"
                      TxKeyBlkRef <$> LDB.deserializeKey


instance LDB.SerializeKey TxKeyDestinationToOutput where

  serializeKey (TxKeyDestinationToOutput dst (OutputRef txId outputIdx)) = do
    SerP.putByteString "txd-"
    LDB.serializeKey dst
    LDB.serializeKey txId
    LDB.serializeKey outputIdx

  deserializeKey = do SerUtil.matchBS "txd-"
                      dst <- LDB.deserializeKey
                      txId <- LDB.deserializeKey
                      outputIdx <- LDB.deserializeKey
                      return $ TxKeyDestinationToOutput dst (OutputRef txId outputIdx)
                      
instance LDB.SerializeKey TxKeyUnspentOutput where

  serializeKey (TxKeyUnspentOutput (OutputRef txId outputIdx)) = do
    SerP.putByteString "txu-"
    LDB.serializeKey txId
    LDB.serializeKey outputIdx

  deserializeKey = do SerUtil.matchBS "txu-"
                      txId <- LDB.deserializeKey
                      outputIdx <- LDB.deserializeKey
                      return $ TxKeyUnspentOutput (OutputRef txId outputIdx)


instance LDB.SerializeKey TxKeySpent where

  serializeKey (TxKeySpent (OutputRef txId inputIdx)) = do
    SerP.putByteString "txs-"
    LDB.serializeKey txId
    LDB.serializeKey inputIdx

  deserializeKey = do SerUtil.matchBS "txs-"
                      txId <- LDB.deserializeKey
                      inputIdx <- LDB.deserializeKey
                      return $ TxKeySpent (OutputRef txId inputIdx)


-- Possible DB-Key-Value combinations
instance LDB.LevelDBKey DBBlockchainIndex BlkKeyProcessedOffset DBOffset 
instance LDB.LevelDBKey DBBlockchainIndex BlkKeyTip Hash256 
instance LDB.LevelDBKey DBBlockchainIndex BlkKeyHeightToBlk Hash256 
instance LDB.LevelDBKey DBBlockchainIndex BlkKeyBlkToHeight Height 
instance LDB.LevelDBKey DBBlockchainIndex BlkKeyBlkByteOffset DBOffset 
instance LDB.LevelDBKey DBBlockchainIndex TxKeyBlkRef TxRef 
instance LDB.LevelDBKey DBBlockchainIndex TxKeyDestinationToOutput DBDestination
instance LDB.LevelDBKey DBBlockchainIndex TxKeyUnspentOutput Hash160
instance LDB.LevelDBKey DBBlockchainIndex TxKeySpent DBInputRef 

-- repeated for mem pool DB-Key-Value combinations
-- instance LDB.LevelDBKey DBBlockchainIndex (TxKeyMemPoolKey TxKeyDestinationToOutput) DBDestination
-- instance LDB.LevelDBKey DBBlockchainIndex (TxKeyMemPoolKey TxKeyUnspentOutput) Hash160
-- instance LDB.LevelDBKey DBBlockchainIndex (TxKeyMemPoolKey TxKeySpent) DBInputRef 

--instance LDB.LevelDBKey DBBlockchainIndex TxKeyMemPoolMember MemPoolTx
