{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module BitD.Protocol.Messages
       ( Version(..)
       , Inv(..)
       , InvType(..)
       , InvVector(..)
       , W64(..)
       , Message(..)
       )
       where

import BitD.Util.Binary (match_)
import BitD.Protocol.VarData (VarInt(..), VarString(..))

import BitD.Protocol.Types (Hash256(..))
import Data.String (IsString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Binary.Get as BinG
import qualified Data.Binary.Put as BinP
import qualified Data.Binary as Bin

import qualified Crypto.Hash.SHA256 as SHA256

import Control.Applicative
import Control.Monad

import Data.Word (Word32, Word64)


magic :: Word32
magic = 0xd9b4bef9

putMagic :: BinP.Put
putMagic = BinP.putWord32le magic

data Version = Version { _version :: !Word32
                       , _services :: !Word64
                       , _timestamp :: !Word64
                       , _addrRecv :: !BS.ByteString
                       , _addrSend :: !BS.ByteString
                       , _verNonce :: !Word64
                       , _userAgent :: !VarString
                       , _startHeight :: !Word32
                       , _relay :: !Bool
                       }
               deriving Show

instance Bin.Binary Version where
  get = Version
        <$> BinG.getWord32le
        <*> BinG.getWord64le
        <*> BinG.getWord64le
        <*> (BinG.skip 26 *> pure (BS.replicate 26 0))
        <*> (BinG.skip 26 *> pure (BS.replicate 26 0))
        <*> BinG.getWord64le
        <*> Bin.get
        <*> BinG.getWord32le
        <*> (go =<< BinG.isEmpty)
    where
      go True = return True
      go False = do
        w <- BinG.getWord8
        return (w == 0)
        
  put (Version v s t _ _ vn ua sh r) = do
    BinP.putWord32le v
    BinP.putWord64le s
    BinP.putWord64le t
    putPad 26
    putPad 26
    BinP.putWord64le vn
    Bin.put ua
    BinP.putWord32le sh
    if r
      then BinP.putWord8 1
      else BinP.putWord8 0

data Inv = Inv ![InvVector]
         deriving Show
                    
instance Bin.Binary Inv where
  get = do
    VarInt c <- Bin.get
    Inv <$> replicateM (fromIntegral c) Bin.get
  put (Inv xs) = do
    Bin.put $ VarInt $ fromIntegral $ length xs
    forM_ xs Bin.put

data InvType = InvError | InvTx | InvBlock
             deriving Show

instance Bin.Binary InvType where
  get = BinG.getWord32le >>= \case
    0 -> return InvError
    1 -> return InvTx
    2 -> return InvBlock
    _ -> fail "invalid inv type"
  put = BinP.putWord32le . \case
    InvError -> 0
    InvTx -> 1
    InvBlock -> 2
    
data InvVector = InvVector { _invType :: !InvType
                           , _invHash :: !Hash256
                           }
                 deriving Show
                 
instance Bin.Binary InvVector where
  get = InvVector <$> Bin.get <*> Bin.get
  put (InvVector t h) = Bin.put t >> Bin.put h

newtype W64 = W64 Word64
              deriving Show

instance Bin.Binary W64 where
  get = W64 <$> BinG.getWord64le
  put (W64 w) = BinP.putWord64le w

  
data Message = MessageNothing !BS.ByteString
             | MessageUnknown !BS.ByteString 
             | MessageVersion Version 
             | MessageVerAck 
             | MessageInv Inv 
             | MessageGetData Inv 
             | MessagePing W64 
             | MessagePong W64
             | MessageTransaction !BS.ByteString
             deriving Show 

putPad :: Int -> BinP.Put
putPad n = BinP.putByteString (BS.replicate n 0)

instance Bin.Binary Message where
  put msg = do
    putMagic
    command <- case msg of
          MessageVersion _ -> return "version"
          MessageVerAck -> return "verack"
          MessageInv _ -> return "inv"
          MessageGetData _ -> return "getdata"
          MessageTransaction _ -> return "tx"
          MessagePing _ -> return "ping"
          MessagePong _ -> return "pong"
          _ -> fail "message name missing"
          
    BinP.putByteString command
    putPad (12 - BS.length command)
    
    payload <- BSL.toStrict <$> case msg of
          MessageVersion ver -> return $ Bin.encode ver
          MessageVerAck -> return ""
          MessageInv v -> return $ Bin.encode v
          MessageGetData v -> return $ Bin.encode v
          MessageTransaction v -> return $ BSL.fromStrict v
          MessagePing v -> return $ Bin.encode v
          MessagePong v -> return $ Bin.encode v
          _ -> fail "message name missing 2"
          
    BinP.putWord32le $ fromIntegral $ BS.length payload
    -- checksum
    BinP.putByteString $ BS.take 4 $ SHA256.hash $ SHA256.hash $ payload
    BinP.putByteString payload
    
  get = do
    match_ BinG.getWord32le magic
    command <- BS.takeWhile (/=0) <$> BinG.getByteString 12
    payloadLength <- fromIntegral <$> BinG.getWord32le
    checksum <- BinG.getByteString 4
    payload <- BinG.getByteString payloadLength
    when (checksum /= (BS.take 4 $ SHA256.hash $ SHA256.hash payload)) $ fail "checksum"

    case command of
      "version" -> return $ MessageVersion (Bin.decode $ BSL.fromStrict payload)
      "verack" -> do
        -- assert (BS.empty payload)
        return MessageVerAck
      "inv" -> return $ MessageInv (Bin.decode $ BSL.fromStrict payload)
      "getdata" -> return $ MessageGetData (Bin.decode $ BSL.fromStrict payload)
      "tx" -> return $ MessageTransaction payload
      "ping" -> return $ MessagePing (Bin.decode $ BSL.fromStrict payload)
      "pong" -> return $ MessagePong (Bin.decode $ BSL.fromStrict payload)
      _ -> return $ MessageUnknown command


-- assert b = unless b $ fail ""
