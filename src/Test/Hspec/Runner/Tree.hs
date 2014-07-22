{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Runner.Tree where

import           Control.Applicative
import           Test.Hspec.Core.Type

data Tree a
  = Node !String [Tree a]
  | Leaf !String a
  deriving (Eq, Show, Functor)

toTree :: Spec -> IO [Tree Item]
toTree spec = concat <$> (runSpecM spec >>= mapM go)
  where
    go x = case x of
      SpecGroup label xs -> return . Node label . concat <$> mapM go xs
      BuildSpecs xs -> concat <$> (xs >>= mapM go)
      SpecItem r item -> return [Leaf r item]
