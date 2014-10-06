{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Test.Hspec.Runner.Tree where

import           Control.Applicative
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)
import           Test.Hspec.Core.Type

data Tree a
  = Node !String [Tree a]
  | NodeWithCleanup (IO ()) [Tree a]
  | Leaf a
  deriving (Functor, Foldable, Traversable)

toTree :: Spec -> IO [Tree (Item ())]
toTree spec = map f <$> runSpecM spec
  where
    f :: SpecTree () -> Tree (Item ())
    f x = case x of
      SpecGroup label xs -> Node label (map f xs)
      SpecWithCleanup cleanup xs -> NodeWithCleanup (cleanup ()) (map f xs)
      SpecItem item -> Leaf item

fromTree :: [Tree (Item ())] -> SpecWith ()
fromTree = fromSpecList . map go
  where
    go :: Tree (Item ()) -> SpecTree ()
    go x = case x of
      Node label xs -> SpecGroup label (map go xs)
      NodeWithCleanup action xs -> SpecWithCleanup (\() -> action) (map go xs)
      Leaf item -> SpecItem item
