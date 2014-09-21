{-# LANGUAGE CPP #-}

module BitD.ConnectionDaemon.AMQP
       ( Message(..)
       ) where

#include <Imports.hs>

import qualified BitD.Util.AMQP as Q
import BitD.Protocol.Types (Hash256(..))

data Message = MsgNewTransaction !BS.ByteString
             | MsgNewBlock !Hash256

instance Q.MsgBody Message where
  serialize (MsgNewTransaction bs) = SerP.putWord8 0 >> Ser.put bs
  serialize (MsgNewBlock hash) = SerP.putWord8 1 >> Ser.put hash
  deserialize = do
    which <- SerG.getWord8
    case which of
      0 -> MsgNewTransaction <$> Ser.get
      1 -> MsgNewBlock <$> Ser.get
      _ -> error "impossible"
