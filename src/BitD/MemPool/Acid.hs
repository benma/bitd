{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module BitD.MemPool.Acid
       ( Acid.AcidState
       , Acid.update
       , Acid.query
       , AcidMemPool(..)
       , addTransaction
       , removeTransaction
       , getTransactions
       , initAcid
       ) where

#include <Imports.hs>

import qualified BitD.Protocol.Blockchain as BC
import BitD.Networks (Network(..))
import BitD.Protocol.Types (TxID(..), toBS)
import BitD.MemPool.HashMap (HashMap(..), hashMap)
import BitD.Util.State (modify')
import qualified Data.SafeCopy as SC
import qualified Data.Acid as Acid

import Control.Monad.Trans.Resource (ResourceT, allocate)

data AcidMemPool = AcidMemPool { _acidTransactions :: HashMap BS.ByteString BS.ByteString }
  
SC.deriveSafeCopy 0 'SC.base ''AcidMemPool

makeLenses ''AcidMemPool

addTransaction' :: BS.ByteString -> BS.ByteString -> Acid.Update AcidMemPool ()
addTransaction' txId rawTx = acidTransactions . at txId ?= rawTx

removeTransaction' :: BS.ByteString -> Acid.Update AcidMemPool ()
removeTransaction' txId = acidTransactions . at txId .= Nothing

countTransactions' :: Acid.Query AcidMemPool Int
countTransactions' = Reader.asks $ \(AcidMemPool (HashMap transactions)) -> Map.size transactions

getTransactions' :: Acid.Query AcidMemPool [BS.ByteString]
getTransactions' = Reader.asks $ \mp -> let HashMap m = mp ^. acidTransactions
                                        in Map.elems m

Acid.makeAcidic ''AcidMemPool ['addTransaction', 'removeTransaction', 'countTransactions', 'getTransactions']


addTransaction :: Acid.AcidState AcidMemPool -> BC.Transaction -> IO ()
addTransaction acid tx = Acid.update acid $ AddTransaction' (toBS $ BC._txHash tx) (BSL.toStrict $ Bin.encode tx)

removeTransaction :: Acid.AcidState AcidMemPool -> BC.Transaction -> IO ()
removeTransaction acid tx = Acid.update acid $ RemoveTransaction' (toBS $ BC._txHash tx)

getTransactions :: Acid.AcidState AcidMemPool -> IO [BC.Transaction]
getTransactions acid = do
  rawTxs <- Acid.query acid GetTransactions'
  return $ map (Bin.decode . BSL.fromStrict) rawTxs
  
acidStatePath :: Network -> IO.FilePath
acidStatePath BTCMain = "/home/ums/bitchange/data/btcmain/mempool"
acidStatePath BTCTest = "/home/ums/bitchange/data/btctest/mempool"


initAcid :: Network -> ResourceT IO (Acid.AcidState AcidMemPool)
initAcid network = snd <$> allocate (Acid.openLocalStateFrom (acidStatePath network) initial) Acid.closeAcidState
  where
    initial = AcidMemPool (HashMap Map.empty)
