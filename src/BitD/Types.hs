{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module BitD.Types
       ( ByteOffset
       )
       where

#include <Imports.hs>

type ByteOffset = Int64
