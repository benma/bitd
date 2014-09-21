module BitD.Util.Binary ( runGet
                        , runGetLazy
                        , match
                        , match_
                        , parseReturnRaw
                        ) where


import Control.Lens (view, _3)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative
import qualified Data.Binary.Get as BinG
import qualified Data.Binary.Put as BinP


runGet :: BinG.Get a -> BS.ByteString -> a
runGet p s = either (error . view _3) (view _3) $ BinG.runGetOrFail p $ BSL.fromStrict s

runGetLazy :: BinG.Get a -> BSL.ByteString -> a
runGetLazy p s = either (error . view _3) (view _3) $ BinG.runGetOrFail p s

match :: Eq a => BinG.Get a -> a -> BinG.Get a
match p test = do
  result <- p
  if result == test
    then return result
    else fail "match failed"

match_ :: Eq a => BinG.Get a -> a -> BinG.Get ()
match_ p test = match p test *> pure ()


parseReturnRaw :: BinG.Get a -> BinG.Get (BS.ByteString, a)
parseReturnRaw g = do
  (len, r) <- BinG.lookAhead $ do
                (res,after) <- BinG.lookAhead $ (,) <$> g <*> BinG.bytesRead
                before <- BinG.bytesRead
                return (after-before, res)
  bs <- BinG.getByteString $ fromIntegral len
  return (bs, r)
