module Test.Hspec.HUnit {-# DEPRECATED "use \"Test.Hspec.Contrib.HUnit\" from package @hspec-contrib@ instead" #-}
(
-- * Interoperability with HUnit
  fromHUnitTest
) where

import           Test.Hspec.Core.Spec
import           Test.HUnit (Test (..))

-- |
-- Convert a HUnit test suite to a spec.  This can be used to run existing
-- HUnit tests with Hspec.
fromHUnitTest :: Test -> Spec
fromHUnitTest t = case t of
  TestList xs -> mapM_ go xs
  x -> go x
  where
    go :: Test -> Spec
    go t_ = case t_ of
      TestLabel s (TestCase e) -> it s e
      TestLabel s (TestList xs) -> describe s (mapM_ go xs)
      TestLabel s x -> describe s (go x)
      TestList xs -> describe "<unlabeled>" (mapM_ go xs)
      TestCase e -> it "<unlabeled>" e
