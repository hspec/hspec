{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Runner.Tree where

import           Control.Applicative
import           Test.Hspec.Core.Type

data Tree a
  = Node !String [Tree a]
  | NodeWithCleanup (IO ()) [Tree a]
  | Leaf !String a
  deriving Functor

toTree :: Spec -> IO [Tree Item]
toTree spec = map f <$> runSpecM spec
  where
    f :: SpecTree -> Tree Item
    f x = case x of
      SpecGroup label xs -> Node label (map f xs)
      SpecWithCleanup cleanup xs -> NodeWithCleanup cleanup (map f xs)
      SpecItem r item -> Leaf r item
