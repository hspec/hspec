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
      TestLabel s (TestCase e)  -> it s e
      TestLabel s (TestList xs) -> describe s (map go xs)
      TestLabel s x             -> describe s [go x]
      TestList xs               -> describe "<unlabeled>" (map go xs)
      TestCase e                -> it  "<unlabeled>" e
