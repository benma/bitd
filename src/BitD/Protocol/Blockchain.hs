{-# LANGUAGE OverloadedStrings #-}

module BitD.Protocol.Blockchain
       ( Transaction(..)
       , Input(..)
       , Output(..)
       , Block(..)
       , BlockData(..)
       , txIdFromRaw
       , parseTransaction
       , parseBlock
       , getBlockData
       , outputScriptToDestinationHash
       , inputScriptToDestinationHash
       , RawTransaction(..)
       , txIso
       )
       where

import BitD.Util.Binary (match, match_, parseReturnRaw, runGet, runGetLazy)

import BitD.Protocol.Types (Hash256(..), TxID(..), SatoshiValue(..), Hash160(..))
import BitD.Networks (Network(..))
import BitD.Protocol.VarData (VarInt(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinG
import qualified Data.Binary.Put as BinP
import qualified Control.Lens as Lens
import qualified Control.Monad as M
import Control.Applicative

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160

import Data.Word (Word32)

type PublicKey = BS.ByteString
type Signature = BS.ByteString


hash160 :: BS.ByteString -> Hash160
hash160 = Hash160 . RIPEMD160.hash . SHA256.hash


data Input = Input { _prevTxHash :: {-# UNPACK #-} !TxID
                   , _prevOutputIndex :: {-# UNPACK #-} !Word32
                   , _inputScriptLength :: {-# UNPACK #-} !Int
                   , _inputScript :: {-# UNPACK #-} !BS.ByteString
                   , _sequenceNumber :: {-# UNPACK #-} !Word32
                   } deriving (Show)
                              
data Output = Output { _value :: {-# UNPACK #-} !SatoshiValue
                     , _outputScriptLength :: {-# UNPACK #-} !Int
                     , _outputScript :: {-# UNPACK #-} !BS.ByteString
                     } deriving (Show)

data DecodedOutputScript = OutputPayToPubKey {-# UNPACK #-} !BS.ByteString
                         | OutputPayToPubKeyHash {-# UNPACK #-} !Hash160
                         | OutputPayToScriptHash {-# UNPACK #-} !Hash160
                         deriving (Show)

data DecodedInputScript = InputSig {-# UNPACK #-} !BS.ByteString
                        | InputSigPubKey {-# UNPACK #-} !Signature {-# UNPACK #-} !PublicKey
                        deriving (Show)

eof :: BinG.Get ()
eof = do
  isEmpty <- BinG.isEmpty
  M.unless isEmpty $ fail ""


decodeOutputScript :: BS.ByteString -> Maybe DecodedOutputScript
decodeOutputScript outputScript = flip runGet outputScript $ Just <$> (payToPubKey <|> payToPubKeyHash <|> payToScriptHash) <|> pure Nothing
  where
    payToPubKey = match_ BinG.getWord8 0x41 -- OP: push 0x41 bytes
                  *> BinG.lookAhead (match_ BinG.getWord8 0x04)
                  *> (OutputPayToPubKey <$> BinG.getByteString 65)
                  <* match_ BinG.getWord8 0xac -- OP_CHECKSIG
                  <* eof
    payToPubKeyHash = match_ BinG.getWord8 0x76 -- OP_DUP
                      *> match_ BinG.getWord8 0xa9 -- OP_HASH160         
                      *> match_ BinG.getWord8 0x14 -- OP: push 0x14 bytes
                      *> (OutputPayToPubKeyHash . Hash160 <$> BinG.getByteString 20)
                      <* match_ BinG.getWord8 0x88 -- OP_EQUALVERIFY
                      <* match_ BinG.getWord8 0xac -- OP_CHECKSIG
                      <* eof
    payToScriptHash = match_ BinG.getWord8 0xa9 -- OP_HASH160
                      *> match_ BinG.getWord8 0x14 -- OP: push 0x14 bytes
                      *> (OutputPayToScriptHash . Hash160 <$> BinG.getByteString 20)
                      <* match_ BinG.getWord8 0x87 -- OP_EQUAL
                      <* eof

outputScriptToDestinationHash :: BS.ByteString -> Maybe Hash160
outputScriptToDestinationHash outputScript = case decodeOutputScript outputScript of
  Just (OutputPayToPubKeyHash hash) -> Just hash
  Just (OutputPayToPubKey pk) -> Just (hash160 pk)
  Just (OutputPayToScriptHash hash) -> Just hash
  Nothing -> Nothing

decodeInputScript :: BS.ByteString -> Maybe DecodedInputScript
decodeInputScript inputScript = flip runGet inputScript $ Just <$> (sigAndPubKey <|> onlySig) <|> pure Nothing
  where sigAndPubKey = do c1 <- BinG.getWord8
                          sig <- BinG.getByteString (fromIntegral c1)
                          c2 <- BinG.getWord8
                          pubKey <- BinG.getByteString (fromIntegral c2)
                          eof
                          return $! InputSigPubKey sig pubKey
        onlySig = do c1 <- BinG.getWord8
                     sig <- BinG.getByteString (fromIntegral c1)
                     eof
                     return $! InputSig sig
          

-- Find the destination of the output script matching this input script.
-- invariant for the case that this function suceeds with Just:
-- inputScriptToDestinationHash inputScript == outputScriptToDestinationHash (outputScript_referenced_by_input)
inputScriptToDestinationHash :: BS.ByteString -> Maybe Hash160
inputScriptToDestinationHash inputScript = case decodeInputScript inputScript of
  Just (InputSigPubKey _ pk) -> Just (hash160 pk)
  _ -> Nothing


data Header = Header { _blockVersion :: {-# UNPACK #-} !Word32
                     , _merkleRootHash :: {-# UNPACK #-} !Hash256
                     , _timestamp :: {-# UNPACK #-} !Word32
                     , _bits :: {-# UNPACK #-} !Word32
                     , _nonce:: {-# UNPACK #-} !Word32
                     } deriving (Show)

data Transaction = Transaction { _txHash :: {-# UNPACK #-} !TxID
                               , _txVersion :: {-# UNPACK #-} !Word32
                               , _inCount :: {-# UNPACK #-} !Int
                               , _inputs :: [(Word32, Input)]
                               , _outCount :: {-# UNPACK #-} !Int
                               , _outputs :: [(Word32, Output)]
                               , _lockTime :: {-# UNPACK #-} !Word32
                               } deriving (Show)
                                          
data BlockData = BlockData { _header :: Header
                           , _txCount :: {-# UNPACK #-} !Int
                           , _transactions :: [Transaction]
                           } deriving (Show)

data Block = Block { _blockHash :: {-# UNPACK #-} !Hash256
                   , _prevHash :: {-# UNPACK #-} !Hash256
                   , _rawBlock :: {-# UNPACK #-} !BS.ByteString
                   } deriving (Show)

parseInput :: BinG.Get Input
parseInput = do
  prevHash <- Bin.get
  prevOutputIndex <- BinG.getWord32le
  VarInt scriptLength <- Bin.get
  script <- BinG.getByteString $ fromIntegral scriptLength
  sequenceNumber <- BinG.getWord32le -- sequence number
  return $ Input prevHash prevOutputIndex (fromIntegral scriptLength) script sequenceNumber

serializeInput :: Input -> BinP.Put
serializeInput (Input prevTxHash prevOutputIndex inputScriptLength inputScript sequenceNumber) = do
  Bin.put prevTxHash
  BinP.putWord32le prevOutputIndex
  Bin.put $ VarInt $ fromIntegral inputScriptLength
  BinP.putByteString inputScript
  BinP.putWord32le sequenceNumber
  

instance Bin.Binary Input where
  get = parseInput
  put = serializeInput

parseOutput :: BinG.Get Output
parseOutput = do
  value <- SatoshiValue <$> BinG.getWord64le
  VarInt scriptLength <- Bin.get
  script <- BinG.getByteString $ fromIntegral scriptLength
  return $ Output value (fromIntegral scriptLength) script

serializeOutput :: Output -> BinP.Put
serializeOutput (Output (SatoshiValue value) outputScriptLength outputScript) = do
  BinP.putWord64le value
  Bin.put $ VarInt $ fromIntegral outputScriptLength
  BinP.putByteString outputScript

instance Bin.Binary Output where
  get = parseOutput
  put = serializeOutput

parseTransaction :: BinG.Get Transaction
parseTransaction = do
  -- transaction version, should be 1, but is 2/garbage for a total of 5 transactions
  (raw, transaction) <- parseReturnRaw $ do txVersion <- BinG.getWord32le
                                            VarInt inCount <- Bin.get
                                            inputs <- zip [0..] <$> M.replicateM (fromIntegral inCount) parseInput
                                            VarInt outCount <- Bin.get
                                            outputs <- zip [0..] <$> M.replicateM (fromIntegral outCount) parseOutput
                                            lockTime <- BinG.getWord32le
                                            return $ Transaction (TxID (Hash256 "")) txVersion (fromIntegral inCount) inputs (fromIntegral outCount) outputs lockTime
  return $ transaction { _txHash = txIdFromRaw raw }

serializeTransaction :: Transaction -> BinP.Put
serializeTransaction (Transaction _ txVersion inCount inputs outCount outputs lockTime) = do
  BinP.putWord32le txVersion
  Bin.put $ VarInt $ fromIntegral inCount
  M.mapM_ (Bin.put . snd) inputs
  Bin.put $ VarInt $ fromIntegral outCount
  M.mapM_ (Bin.put . snd) outputs
  BinP.putWord32le lockTime

instance Bin.Binary Transaction where
  get = parseTransaction
  put = serializeTransaction


txIdFromRaw :: BS.ByteString -> TxID
txIdFromRaw = TxID . Hash256 . SHA256.hash . SHA256.hash

parseHeader :: Network -> BinG.Get Header
parseHeader network = Header
                      <$> blockVersion network
                      <* (Bin.get :: BinG.Get Hash256) -- prevHash
                      <*> Bin.get -- merkleRootHash
                      <*> BinG.getWord32le -- timestamp
                      <*> BinG.getWord32le -- bits
                      <*> BinG.getWord32le -- nonce
  where
    blockVersion BTCMain = (match BinG.getWord32le 0x01 <|> match BinG.getWord32le 0x02)
    -- wonky block versions exist on the testnet (e.g. in block 244724)
    blockVersion BTCTest = BinG.getWord32le

getMagic BTCMain = 0xd9b4bef9
getMagic BTCTest = 0x0709110b

parseBlock :: Network -> BinG.Get Block
parseBlock network = do
  match_ BinG.getWord32le $ getMagic network
  blockSize <- BinG.getWord32le
  rawHeader <- BS.copy <$> (BinG.lookAhead $ BinG.getByteString 80)
  let blockHash = Hash256 . SHA256.hash . SHA256.hash $! rawHeader
  let prevHash = runGet (BinG.skip 4 *> Bin.get) $! rawHeader
  blob <- BinG.getByteString (fromIntegral blockSize)
  return $! Block blockHash prevHash blob

getBlockData :: Network -> Block -> BlockData
getBlockData network block = flip runGet (_rawBlock block) $ do
  header <- parseHeader network
  VarInt txCount <- Bin.get
  rest <- BinG.getRemainingLazyByteString
  let txs = flip runGetLazy rest $ M.replicateM (fromIntegral txCount) parseTransaction
  return $ BlockData header (fromIntegral txCount) txs
{-# INLINE getBlockData #-}

data RawTransaction = RawTransaction !BS.ByteString
                    deriving (Show)

txIso :: Lens.Iso' RawTransaction Transaction
txIso = Lens.iso from' to'
  where
    from' (RawTransaction rawTx) = Bin.decode (BSL.fromStrict rawTx)
    to' tx = RawTransaction $ BSL.toStrict $ Bin.encode tx
