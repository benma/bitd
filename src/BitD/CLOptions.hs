{-# LANGUAGE CPP #-}
module BitD.CLOptions
       ( CLOptions(..)
       , parseCLOptions
       )
       where

#include <Imports.hs>

import qualified Options.Applicative as O

import BitD.Networks (Network(..), networkToString, networkFromString)

data CLOptions = CLOptions { _network :: Network
                             
                           }
               deriving (Show)


fromStringOption fromString m = O.nullOption $ O.reader enum <> m
  where enum = maybe (fail "enumoption") return . fromString
                     
parseCLOptions :: IO CLOptions
parseCLOptions = O.execParser parserInfo
  where
    parserInfo = O.info (O.helper <*> parser) (O.progDesc "BitD Daemon Application.")
    
    parser = CLOptions
             <$> fromStringOption networkFromString (O.long "net" <> O.help "Which net? [btcmain|btctest]" <> O.value BTCMain <> O.showDefaultWith networkToString)
