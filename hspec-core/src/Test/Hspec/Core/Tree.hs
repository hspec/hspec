{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Tree (
  Tree (..)
) where

import           Control.Applicative
import           Data.Foldable
import           Data.Traversable
import           Data.Monoid

-- | Internal representation of a spec.
data Tree c a =
    Node String [Tree c a]
  | NodeWithCleanup c [Tree c a]
  | Leaf a
  deriving Functor

instance Foldable (Tree c) where -- Note: GHC 7.0.1 fails to derive this instance
  foldMap = go
    where
      go :: Monoid m => (a -> m) -> Tree c a -> m
      go f t = case t of
        Node _ xs -> foldMap (foldMap f) xs
        NodeWithCleanup _ xs -> foldMap (foldMap f) xs
        Leaf x -> f x

instance Traversable (Tree c) where -- Note: GHC 7.0.1 fails to derive this instance
  sequenceA = go
    where
      go :: Applicative f => Tree c (f a) -> f (Tree c a)
      go t = case t of
        Node label xs -> Node label <$> sequenceA (map go xs)
        NodeWithCleanup action xs -> NodeWithCleanup action <$> sequenceA (map go xs)
        Leaf a -> Leaf <$> a
