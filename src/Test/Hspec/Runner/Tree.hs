{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Runner.Tree where

import           Test.Hspec.Core.Type

data Tree a
  = Node String [Tree a]
  | Leaf String a
  deriving (Eq, Show, Functor)

toTree :: SpecTree () -> Tree (Item ())
toTree spec = case spec of
  SpecGroup label specs -> Node label (map toTree specs)
  SpecItem r item -> Leaf r item
