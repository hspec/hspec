{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module NonEmpty (
  NonEmpty(..)
, nonEmpty
, reverse
#ifdef TEST
, fromList
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (reverse)

import qualified Data.List as List
import qualified Data.Foldable as Foldable

data NonEmpty a = a :| [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

infixr 5 :|

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (a:as) = Just (a :| as)

reverse :: NonEmpty a -> NonEmpty a
reverse = lift List.reverse

lift :: Foldable f => ([a] -> [b]) -> f a -> NonEmpty b
lift f = fromList . f . Foldable.toList

fromList :: [a] -> NonEmpty a
fromList (a:as) = a :| as
fromList [] = error "NonEmpty.fromList: empty list"
