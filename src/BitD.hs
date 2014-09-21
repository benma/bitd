{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#include <Imports.hs>


import qualified System.Log.Logger as Logger
import BitD.CLOptions (CLOptions(..), parseCLOptions)
-- import BitD.ShowBalance (showBalance)
import qualified BitD.Sync as Sync
import BitD.Protocol.Types (toHex)
import qualified BitD.Protocol.Blockchain as BC
import Control.Monad.Trans.Resource (runResourceT)
import qualified BitD.ConnectionDaemon as CD

import qualified Network.AMQP as Q

main :: IO ()
main = do
  clOptions <- parseCLOptions
  Logger.updateGlobalLogger Logger.rootLoggerName (Logger.setLevel Logger.DEBUG)
  runResourceT $ do
    --CD.daemon
    Sync.daemon $ _network clOptions
  --showBalance
