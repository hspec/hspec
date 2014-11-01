{-# LANGUAGE CPP, DeriveFunctor #-}
module Test.Hspec.Runner.Tree where

import           Control.Applicative
import           Data.Foldable
import           Data.Traversable
import           Test.Hspec.Core.Type
import           Data.Monoid

data Tree a
  = Node !String [Tree a]
  | NodeWithCleanup (IO ()) [Tree a]
  | Leaf a
  deriving Functor

instance Foldable Tree where -- Note: GHC 7.0.1 fails to derive this instance
  foldMap = go
    where
      go :: Monoid m => (a -> m) -> Tree a -> m
      go f t = case t of
        Node _ xs -> foldMap (foldMap f) xs
        NodeWithCleanup _ xs -> foldMap (foldMap f) xs
        Leaf x -> f x

instance Traversable Tree where -- Note: GHC 7.0.1 fails to derive this instance
  sequenceA = go
    where
      go :: Applicative f => Tree (f a) -> f (Tree a)
      go t = case t of
        Node label xs -> Node label <$> sequenceA (map go xs)
        NodeWithCleanup action xs -> NodeWithCleanup action <$> sequenceA (map go xs)
        Leaf a -> Leaf <$> a

toTree :: Spec -> IO [Tree Item]
toTree spec = map f <$> runSpecM spec
  where
    f :: SpecTree -> Tree Item
    f x = case x of
      SpecGroup label xs -> Node label (map f xs)
      SpecWithCleanup cleanup xs -> NodeWithCleanup cleanup (map f xs)
      SpecItem item -> Leaf item

fromTree :: [Tree Item] -> Spec
fromTree = fromSpecList . map go
  where
    go :: Tree Item -> SpecTree
    go x = case x of
      Node label xs -> SpecGroup label (map go xs)
      NodeWithCleanup action xs -> SpecWithCleanup action (map go xs)
      Leaf item -> SpecItem item
