{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module BitD.BitcoinD.RPC ( rpcCall
                         , rpcGetBlockHash
                         , rpcGetBlockCount
                         , rpcGetRawMemPool
                         , rpcGetRawMemPool'
                         , rpcGetRawTransaction
                         , UTF8BS(..)
                         ) where

import BitD.Prelude
import BitD.Networks (Network(..))
import BitD.Protocol.Types (Hash256Hex(..), TxID(..), toBS, toHex, fromHex)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AT
import Data.Aeson ((.=), (.:))
import qualified Control.Monad as M
import Data.Maybe (catMaybes)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data Options = Options { _rpcUser :: String
                       , _rpcPassword :: String
                       , _rpcUrl :: String
                       }

-- | Parameters to connect to bitcoind.
-- TODO: do not hardcode, connect to a config system. 
getOptions :: Network -> Options
getOptions BTCMain = Options "bitcoinrpc" "4HixkVrDAE8B81DnS4PVrVFqEXaY5jrQBwei7CeobSwc" "http://127.0.0.1:8332/"
getOptions BTCTest = Options "bitcoinrpc" "4HixkVrDAE8B81DnS4PVrVFqEXaY5jrQBwei7CeobSwc" "http://127.0.0.1:18332/"

rpcCall :: (JSON.ToJSON p, JSON.FromJSON r) => Network -> String -> [p] -> IO (Maybe r)
rpcCall network method params = do
  let options = getOptions network
  (code, ret, _) <- readProcessWithExitCode "curl" [
    "-s",
    "--user",
    (_rpcUser options ++ ":" ++ _rpcPassword options),
    "--data-binary",
    BSLC.unpack $ JSON.encode $ JSON.object ["jsonrpc" .= JSON.String "1.0"
                                      ,"id" .= JSON.String "bitd"
                                      ,"method" .= method
                                      ,"params" .= params
                                      ],
    "-H",
    "content-type: text/plain;",
    _rpcUrl options
    ] ""
  return $ case code of
    ExitFailure _ -> Nothing
    ExitSuccess -> do
      decoded <- JSON.decodeStrict' $ BSC.pack ret
      flip AT.parseMaybe decoded $ \obj ->
        obj .: "error" >>= \case
          JSON.Null -> obj .: "result"
          _ -> M.mzero
    

noParams :: [()]
noParams = []

newtype UTF8BS = UTF8BS BS.ByteString
               deriving (Show)

fromUTF8BS :: UTF8BS -> BS.ByteString
fromUTF8BS (UTF8BS bs) = bs

instance JSON.FromJSON UTF8BS where
  parseJSON (JSON.String s) = return $ UTF8BS $ encodeUtf8 s
  parseJSON _ = M.mzero

instance JSON.ToJSON UTF8BS where
  toJSON (UTF8BS s) = JSON.String $ decodeUtf8 s

rpcGetBlockHash :: Network -> Word32 -> IO (Maybe Hash256Hex)
rpcGetBlockHash network index = do
  result <- rpcCall network "getblockhash" [index]
  case result of
    Nothing -> return Nothing
    Just (UTF8BS bs) -> return $ Just $ Hash256Hex bs

rpcGetBlockCount :: Network -> IO (Maybe Integer)
rpcGetBlockCount network = rpcCall network "getblockcount" noParams

rpcGetRawMemPool' :: Network -> IO (Maybe [TxID])
rpcGetRawMemPool' network = do
  txHashes <- rpcCall network "getrawmempool" noParams :: IO (Maybe [UTF8BS])
  return $ (map (fromHex . Hash256Hex . fromUTF8BS) <$> txHashes)

rpcGetRawTransaction :: Network -> TxID -> IO (Maybe BS.ByteString)
rpcGetRawTransaction network txHash = do
  rpcCall network "getrawtransaction" [UTF8BS $ toHex txHash] >>= \case
    Nothing -> return Nothing
    Just (UTF8BS rawTxHex) -> return $ Just $ fst $ B16.decode rawTxHex
  
rpcGetRawMemPool :: Network -> IO (Maybe [BS.ByteString])
rpcGetRawMemPool network = do
  txHashes <- rpcGetRawMemPool' network
  case txHashes of
    Nothing -> return Nothing
    Just txHashes' -> do
      rawTxs <- M.forM txHashes' $ \txHash -> rpcCall network "getrawtransaction" [UTF8BS $ toHex txHash]
      return $ Just $ map (\(UTF8BS rawTxHex) -> fst $ B16.decode rawTxHex) $ catMaybes rawTxs
