{-# LANGUAGE CPP #-}
module DList where

import           Prelude ()
import           Test.Hspec.Core.Compat

newtype DList a = DList ([a] -> [a])

instance Monoid (DList a) where
  mempty = DList id
#if MIN_VERSION_base(4,11,0)
instance Semigroup (DList a) where
#endif
  DList f
#if MIN_VERSION_base(4,11,0)
    <>
#else
    `mappend`
#endif
    DList g = DList (f . g)

singleton :: a -> DList a
singleton x = DList (x :)

fromList :: [a] -> DList a
fromList xs = DList (xs ++)

toList :: DList a -> [a]
toList (DList f) = f []
