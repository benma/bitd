-- adapted code from the Haskoin project

module BitD.Protocol.BitcoinAddress
       ( addressToHash160
       , hash160ToAddress
       , AddressVersion(..)
       )
       where

import BitD.Protocol.Types (Hash256(..), Hash160(..), Address(..))
import qualified Crypto.Hash.SHA256 as SHA256
import Control.Monad (guard)
import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import Data.Bits (shiftR, shiftL, (.|.))
import Data.Maybe (fromJust)
import Data.List (unfoldr)
import Data.Word (Word8)
import Data.Char (ord)

checkSum :: BS.ByteString -> BS.ByteString
checkSum = BS.take 4 . SHA256.hash . SHA256.hash

b58String :: String
b58String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Data :: BS.ByteString
b58Data = BS.pack $ map (fromIntegral . ord) b58String

b58Data' :: Map.HashMap Word8 Int
b58Data' = Map.fromList $ zip (BS.unpack b58Data) [0..57]

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> Map.lookup w b58Data'

bsToInteger :: BS.ByteString -> Integer
bsToInteger = (foldr f 0) . reverse . BS.unpack
  where
    f w n = (toInteger w) .|. shiftL n 8

integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0 = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
    where f 0 = Nothing
          f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

encodeBase58I :: Integer -> BS.ByteString
encodeBase58I 0 = BS.pack [b58 0]
encodeBase58I i
    | i >= 0 = go BS.empty i
    | otherwise = error "encodeBase58 is not defined for negative Integers"
  where
    go acc 0 = acc
    go acc n = go (BS.cons (fromIntegral b) acc) q
      where
        (q,r) = n `quotRem` 58
        b = b58 $ fromIntegral r

encodeBase58 :: BS.ByteString -> BS.ByteString
encodeBase58 bs = BS.append l r
  where
    (z,b) = BS.span (== 0) bs
    l = BS.map b58 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = encodeBase58I $ bsToInteger b
          
decodeBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBase58 bs = r >>= return . BS.append prefix
  where (z,b) = BS.span (== (b58 0)) bs
        prefix = BS.map (fromJust . b58') z -- preserve leading 1's
        r | BS.null b = Just BS.empty
          | otherwise = integerToBS <$> foldl f (Just 0) (BS.unpack b)
        f i w = do
            n <- fromIntegral <$> b58' w
            p <- i
            return $ p*58 + n

decodeBase58Check :: BS.ByteString -> Maybe BS.ByteString
decodeBase58Check bs = do
  rs <- decodeBase58 bs
  let (res,chk) = BS.splitAt (BS.length rs - 4) rs
  guard $ chk == checkSum res
  return res

encodeBase58Check :: BS.ByteString -> BS.ByteString
encodeBase58Check bs = encodeBase58 $ BS.append bs chk
  where chk = checkSum bs

addressToHash160 :: Address -> Maybe Hash160
addressToHash160 (Address s) = (Hash160 . BS.drop 1) <$> decodeBase58Check s

data AddressVersion = VersionPayToAddress
                    | VersionPayToScriptHash

-- main net
addressVersionToVersionByte :: AddressVersion -> Word8
addressVersionToVersionByte VersionPayToAddress = 0
addressVersionToVersionByte VersionPayToScriptHash = 5

hash160ToAddress :: Hash160 -> AddressVersion -> Address
hash160ToAddress (Hash160 h) version = Address $ encodeBase58Check ((addressVersionToVersionByte version) `BS.cons` h)
