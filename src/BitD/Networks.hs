module BitD.Networks
       ( Network(..)
       , networkToString
       , networkFromString
       )
       where

data Network = BTCMain
             | BTCTest


networkToString BTCMain = "btcmain"
networkToString BTCTest = "btctest"

instance Show Network where
  show = networkToString

networkFromString "btcmain" = Just BTCMain
networkFromString "btctest" = Just BTCTest
networkFromString _ = Nothing
