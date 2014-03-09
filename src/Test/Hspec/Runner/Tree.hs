module Test.Hspec.Runner.Tree where

import           Test.Hspec.Core.Type

data Tree a
  = Node String [Tree a]
  | Leaf String a
  deriving (Eq, Show)

instance Functor Tree where
  fmap f t = case t of
    Node s xs -> Node s (map (fmap f) xs)
    Leaf s x -> Leaf s (f x)

toTree :: SpecTree () -> Tree (Item ())
toTree spec = case spec of
  SpecGroup label specs -> Node label (map toTree specs)
  SpecItem r item -> Leaf r item
