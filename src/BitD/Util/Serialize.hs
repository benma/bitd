{-# LANGUAGE CPP #-}

module BitD.Util.Serialize ( matchBS
                           ) where

#include <Imports.hs>

matchBS :: BS.ByteString -> SerG.Get ()
matchBS bs = do
  bs' <- Ser.getByteString (BS.length bs)
  if bs == bs'
    then return ()
    else fail ""

