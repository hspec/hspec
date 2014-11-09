module Test.Hspec.HUnit (
-- * Interoperability with HUnit
  fromHUnitTest
) where

import           Test.Hspec.Core.Type
import           Test.HUnit (Test (..))

-- |
-- Convert a HUnit test suite to a spec.  This can be used to run existing
-- HUnit tests with Hspec.
fromHUnitTest :: Test -> Spec
fromHUnitTest t = fromSpecList $ case t of
  TestList xs -> map go xs
  x           -> [go x]
  where
    go :: Test -> SpecTree
    go t_ = case t_ of
      TestLabel s (TestCase e)  -> specItem s e
      TestLabel s (TestList xs) -> specGroup s (map go xs)
      TestLabel s x             -> specGroup s [go x]
      TestList xs               -> specGroup "<unlabeled>" (map go xs)
      TestCase e                -> specItem  "<unlabeled>" e
