{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module BitD.ConnectionDaemon
       ( Peer(..)
       , daemon
       , streamTxFromNode
       , connect
       ) where

#include <Imports.hs>

import BitD.Util.Daemon (async)
import qualified BitD.Util.AMQP as Q
import qualified BitD.ConnectionDaemon.AMQP as QM
import Control.Monad.Trans.Resource (ResourceT)
import qualified Network.AMQP as QQ
import BitD.Util.Binary (runGet)
import qualified BitD.Protocol.Blockchain as BC
import BitD.Protocol.Messages (Version(..), Message(..), Inv(..), InvVector(..), InvType(..))
import qualified System.Random.MWC as Random
import Data.Time.Clock.POSIX (getPOSIXTime)



data Peer = Peer { _hostname :: Net.HostName
                 , _port :: Net.PortNumber
                 } deriving (Show)
                       
data Connection = Connection { _connection_handle :: !IO.Handle, _connection_nonce :: !Word64 }


defaultHost :: Net.HostName
defaultHost = "127.0.0.1"

defaultPort :: Net.PortNumber
defaultPort = 8333

connect :: Peer -> IO Connection
connect (Peer host port) = do
  gen <- Random.withSystemRandom . Random.asGenIO $ return
  nonce <- Random.uniform gen
  
  handle <- Net.connectTo host (Net.PortNumber port)
  IO.hSetBuffering handle IO.NoBuffering
  
  let conn = Connection handle nonce
  sendVersion conn
  return conn
  where
    sendVersion conn = do
      ts <- round <$> getPOSIXTime
      let ver =  (Version
                  70002
                  1
                  ts
                  ""
                  ""
                  (_connection_nonce conn)
                  "/bitd:0.1/"
                  0
                  True
                 )
      sendMsg conn (MessageVersion ver)

streamMsgsFromNode :: Connection -> P.Producer Message IO ()
streamMsgsFromNode conn = do
  
  let handle = _connection_handle conn
  let rawInput = PBS.fromHandle handle
  let streamMsgs = PL.evalStateP rawInput $ M.forever $
                   either (const $ error "parser fail") P.yield =<< lift PB.decode

  streamMsgs P.>-> handleMsg
  where
    handleMsg = do
      P.await >>= \case
        MessageVersion ver -> lift $ print ver
        _ -> fail "expected version message"

      M.forever $ do
        msg <- P.await
        case msg of
          MessageInv inv -> sendMsg conn $ MessageGetData inv
          MessagePing nonce -> do lift $ print ("received ping!!" :: String)
                                  sendMsg conn $ MessagePong nonce
          -- MessageUnknown command -> do lift $ print ("received unknown message: " `BS.append` command)
          _ -> return ()
        P.yield msg

sendMsg :: (PB.Binary a, MonadIO m) => Connection -> a -> m ()
sendMsg conn msg = P.runEffect $ PB.encode msg P.>-> PBS.toHandle (_connection_handle conn)

streamTxFromNode :: Connection -> P.Producer BC.Transaction IO ()
streamTxFromNode conn = P.for (streamMsgsFromNode conn) extract
  where
    extract (MessageTransaction raw) = P.yield $ runGet BC.parseTransaction raw
    extract _ = return ()

routingKey :: T.Text
routingKey = "inv"

daemon :: ResourceT IO ()
daemon = do
  conn <- lift $ connect (Peer defaultHost defaultPort)
  chan <- Q.open

  async $ P.runEffect $ P.for (streamMsgsFromNode conn) (handleMsg chan)
    
  
  where
    handleMsg chan (MessageTransaction raw) = do
      -- lift $ print "tx!"
      lift $ Q.publishMsg chan routingKey $ QM.MsgNewTransaction raw
    handleMsg chan (MessageInv (Inv xs)) = F.forM_ xs $ \(InvVector invType invHash) -> do
      case invType of
        InvError -> return ()
        InvTx -> return ()
        InvBlock -> do
          --lift $ print "block!"
          lift $ Q.publishMsg chan routingKey $ QM.MsgNewBlock invHash
    handleMsg _ _ = return ()
