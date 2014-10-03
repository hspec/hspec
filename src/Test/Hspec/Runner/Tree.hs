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
toTree spec = concat <$> (runSpecM spec >>= mapM go)
  where
    go :: SpecTree -> IO [Tree Item]
    go x = case x of
      SpecGroup label xs -> return . Node label . concat <$> mapM go xs
      BuildSpecs xs -> concat <$> (xs >>= mapM go)
      SpecWithCleanup cleanup y -> return . NodeWithCleanup cleanup <$> go y
      SpecItem r item -> return [Leaf r item]
