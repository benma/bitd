{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module BitD.DB.Types
       ( OutputRef(..), InputRef(..)
       , Balance(..)
       )
       where

#include <Imports.hs>
import BitD.Protocol.Types (TxID(..), SatoshiValue(..))

data OutputRef = OutputRef TxID Word32
               deriving (Bounded, Eq)

instance Hashable OutputRef where
  hashWithSalt salt (OutputRef txId outputIdx) = hashWithSalt salt (txId, outputIdx)
                        
data InputRef = InputRef TxID Word32
                deriving (Eq, Show)

data Balance = Balance { _confirmed :: SatoshiValue
                       , _unconfirmed :: SatoshiValue
                       } 

instance Monoid Balance where
  mempty = Balance 0 0
  Balance a1 b1 `mappend` Balance a2 b2 = Balance (a1+a2) (b1+b2)
