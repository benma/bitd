{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module BitD.Protocol.Types
       ( Hash256(..)
       , Hash256Hex(..)
       , Hash160(..)
       , Address(..)
       , ToBS(..)
       , TxID(..)
       , SatoshiValue(..)
       , Hex(..)
       )
       where

#include <Imports.hs>

import BitD.Util.LevelDB (SerializeKey(..), SerializeValue(..))
import qualified Data.ByteString.Base16 as B16
import Data.String (IsString)
import qualified Data.Aeson as JSON
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

class ToBS a where
  toBS :: a -> BS.ByteString

instance ToBS BS.ByteString where
  toBS = id


newtype Hash256 = Hash256 BS.ByteString
                deriving (Eq, Show, ToBS, Hashable)


newtype Hash160 = Hash160 BS.ByteString
                deriving (Eq, Show, ToBS, Hashable)

newtype Hash256Hex = Hash256Hex BS.ByteString
                   deriving (Eq, Show, ToBS, Bin.Binary, IsString)

newtype Address = Address BS.ByteString
                  deriving (Eq, Show, ToBS)

newtype TxID = TxID Hash256
             deriving (Eq, ToBS, Ser.Serialize, Bin.Binary, Bounded, Hashable)


instance Show TxID where
  show (TxID h) = BSC.unpack $ toHex h

instance Bounded Hash256 where
  minBound = Hash256 $ BS.replicate 32 0
  maxBound = Hash256 $ BS.replicate 32 255

instance Bounded Hash160 where
  minBound = Hash160 $ BS.replicate 20 0
  maxBound = Hash160 $ BS.replicate 20 255

instance Bin.Binary Hash256 where
  get = Hash256 <$> BinG.getByteString 32
  put (Hash256 h) = BinP.putByteString h 

instance Ser.Serialize Hash256 where
  get = Hash256 <$> SerG.getByteString 32
  put (Hash256 h) = SerP.putByteString h 

instance Bin.Binary Hash160 where
  get = Hash160 <$> BinG.getByteString 20
  put (Hash160 h) = BinP.putByteString h 

instance Ser.Serialize Hash160 where
  get = Hash160 <$> SerG.getByteString 20
  put (Hash160 h) = SerP.putByteString h 

newtype SatoshiValue = SatoshiValue Word64
                     deriving (Show, Num, JSON.ToJSON)
                              

instance SerializeValue Hash256 where
  serializeValue = Ser.put
  deserializeValue = Ser.get

instance SerializeValue Hash160 where
  serializeValue = Ser.put
  deserializeValue = Ser.get

instance SerializeValue TxID where
  serializeValue (TxID txHash) = Ser.put txHash
  deserializeValue = TxID <$> Ser.get

instance SerializeKey Hash256 where
  serializeKey = Ser.put
  deserializeKey = Ser.get

instance SerializeKey TxID where
  serializeKey (TxID v) = Ser.put v
  deserializeKey = TxID <$> Ser.get

instance SerializeKey Hash160 where
  serializeKey = Ser.put
  deserializeKey = Ser.get

class Hex a b where
  toHex :: a -> b
  fromHex :: b -> a

instance Hex Hash160 BS.ByteString where
  toHex (Hash160 h) = B16.encode h
  fromHex h = Hash160 $ fst $ B16.decode h

instance Hex Hash256 BS.ByteString where
  toHex (Hash256 h) = B16.encode $ BS.reverse h
  fromHex h = Hash256 $ BS.reverse $ fst $ B16.decode h

instance Hex Hash256 Hash256Hex where
  toHex = Hash256Hex . toHex
  fromHex (Hash256Hex h) = fromHex h

instance Hex TxID Hash256Hex where
  toHex (TxID h) = toHex h
  fromHex h = TxID (fromHex h)

instance Hex TxID BS.ByteString where
  toHex (TxID h) = toHex h
  fromHex h = TxID (fromHex h)

instance JSON.ToJSON Hash256Hex where
  toJSON (Hash256Hex h) = JSON.String $ decodeUtf8 h
