{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module BitD.WebService
       ( webservice
       )
       where

#include <Imports.hs>

import BitD.DB.Types (Balance(..))
import BitD.Protocol.Types (Address(..), Hash256Hex(..), toHex)
import BitD.Protocol.BitcoinAddress (addressToHash160)
import qualified BitD.MemPool as MP
import qualified BitD.DB as DB
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import qualified Snap.Core as Snap
import qualified Snap.Http.Server as SnapS
import qualified Snap.Http.Server.Config as SnapSC

import Data.Text.Encoding (decodeUtf8)

import qualified Data.Aeson as JSON

webservice :: MP.MemPool -> DB.DBBlockchainIndex -> IO ()
webservice memPool db = do
  liftIO $ SnapS.httpServe config site
  where
    config = SnapSC.setVerbose False $
             SnapSC.setErrorLog SnapSC.ConfigNoLog $
             SnapSC.setAccessLog SnapSC.ConfigNoLog $
             SnapSC.setPort port $
             SnapSC.setHostname host $
             SnapSC.setBind bind $
             SnapSC.defaultConfig
    port = 1234
    host = "localhost"
    bind = "127.0.0.1"
    site = jsonHandler (serve memPool db) <|> Snap.writeBS "error"

    jsonHandler = Snap.method Snap.GET . matchMime "application/json"

matchMime :: BS.ByteString -> Snap.Snap a -> Snap.Snap a
matchMime mime action = do
  request <- Snap.getRequest
  let acceptHeader = Snap.getHeaders "Accept" request
  -- check application/json
  action

serve :: MP.MemPool -> DB.DBBlockchainIndex -> Snap.Snap ()
serve memPool db = do
  Snap.modifyResponse $ Snap.setContentType "application/json"
  jsonResponse $ Snap.route [ ("block/tip", serveBlockTip memPool db)
                            , ("address/:address", serveBalance memPool db)
                            ]

jsonResponse :: (JSON.ToJSON r) => Snap.Snap r -> Snap.Snap ()
jsonResponse action = action >>= (Snap.writeLBS . JSON.encode)

serveBalance :: MP.MemPool -> DB.DBBlockchainIndex -> Snap.Snap JSON.Value
serveBalance memPool db = do
  Snap.getParam "address" >>= \case
    Nothing -> serveError
    Just address -> case addressToHash160 (Address address) of
      Nothing -> serveError
      Just dst -> do
        Balance confirmed unconfirmed <- runDB $ DB.getBalance memPool db dst
        return $ JSON.object [ "balance" .:= confirmed
                             , "unconfirmed" .:= unconfirmed
                             ]

serveError = Snap.pass

serveBlockTip :: MP.MemPool -> DB.DBBlockchainIndex -> Snap.Snap JSON.Value
serveBlockTip memPool db = do
  (runDB $ DB.getTip db) >>= \case
    Nothing -> return $ JSON.object []
    Just (height, hash) ->
      return $ JSON.object [ "height" .:= JSON.toJSON height
                           , "hash" .:= JSON.toJSON (toHex hash :: Hash256Hex)]


(.:=) :: (JSON.ToJSON v) => T.Text -> v -> (T.Text, JSON.Value)
(.:=) = (JSON..=)

runDB = liftIO . runResourceT
