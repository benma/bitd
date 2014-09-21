{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE CPP #-}
module BitD.MemPool.HashMap
       ( HashMap(..)
       , hashMap
       ) where

#include <Imports.hs>

import qualified Data.SafeCopy as SC

-- wrap Map.HashMap in order to write a SafeCopy instance.

newtype HashMap k v = HashMap (Map.HashMap k v)
                    deriving (Show)

Lens.makeIso ''HashMap

type instance Index (HashMap k v) = k
type instance Lens.IxValue (HashMap k a) = a

instance (Eq k, Hashable k) => Lens.Ixed (HashMap k a) where
  ix k f (HashMap m) = HashMap <$> ix k f m

instance (Eq k, Hashable k) => Lens.At (HashMap k v) where
  at k f (HashMap m) = HashMap <$> at k f m

instance (SC.SafeCopy k, SC.SafeCopy v, Eq k, Hashable k) => SC.SafeCopy (HashMap k v) where
  getCopy = SC.contain $ HashMap <$> fmap Map.fromList SC.safeGet
  putCopy (HashMap m) = SC.contain $ SC.safePut $ Map.toList m
