{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BitD.Protocol.VarData
       ( VarInt(..)
       , VarString(..)
       ) where

import qualified Data.ByteString as BS
import Data.String (IsString)

import qualified Data.Binary.Get as BinG
import qualified Data.Binary.Put as BinP
import qualified Data.Binary as Bin

import qualified Data.Serialize.Get as SerG
import qualified Data.Serialize.Put as SerP
import qualified Data.Serialize as Ser

import Control.Applicative ((<$>))

import Data.Word (Word64)

newtype VarInt = VarInt Word64

instance Bin.Binary VarInt where
  get = VarInt <$> (BinG.getWord8 >>= go)
    where go w | w < 0xfd = return $ fromIntegral w
               | w == 0xfd = fromIntegral <$> BinG.getWord16le
               | w == 0xfe = fromIntegral <$> BinG.getWord32le
               | w == 0xff = fromIntegral <$> BinG.getWord64le
               | otherwise = fail ""
  put (VarInt v) | v < 0xfd = BinP.putWord8 $ fromIntegral v
                 | v <= 0xffff = do BinP.putWord8 0xfd
                                    BinP.putWord16le $ fromIntegral v
                 | v <= 0xffffffff = do BinP.putWord8 0xfe
                                        BinP.putWord32le $ fromIntegral v
                 | otherwise = do BinP.putWord8 0xff
                                  BinP.putWord64le v

instance Ser.Serialize VarInt where
  get = Bin.decode <$> (SerG.remaining >>= (SerG.getLazyByteString . fromIntegral))
  put = SerP.putLazyByteString . Bin.encode
  
newtype VarString = VarString BS.ByteString
                  deriving (Show, IsString)

instance Bin.Binary VarString where
  get = do
    VarInt len <- Bin.get
    VarString <$> BinG.getByteString (fromIntegral len)
  put (VarString bs) = do
    Bin.put $ VarInt $ fromIntegral $ BS.length bs
    BinP.putByteString bs
