-- |
-- maintainer: Simon Hengel <sol@typeful.net>
module Test.Hspec.Contrib.HUnit (
-- * Interoperability with HUnit
  fromHUnitTest
, specListFromHUnitTest
) where

import           Test.Hspec.Core.Spec
import           Test.HUnit (Test (..))

-- |
-- Convert a HUnit test suite to a spec.  This can be used to run existing
-- HUnit tests with Hspec.
fromHUnitTest :: Test -> Spec
fromHUnitTest = fromSpecList . specListFromHUnitTest

-- |
-- @specListFromHUnitTest@ is similar to `fromHUnitTest`, but it constructs a
-- list of `SpecTree`s instead of a `Spec`.
specListFromHUnitTest :: Test -> [SpecTree ()]
specListFromHUnitTest t = case t of
  TestList xs -> map go xs
  x -> [go x]
  where
    go :: Test -> SpecTree ()
    go t_ = case t_ of
      TestLabel s (TestCase e) -> specItem s e
      TestLabel s (TestList xs) -> specGroup s (map go xs)
      TestLabel s x -> specGroup s [go x]
      TestList xs -> specGroup "<unlabeled>" (map go xs)
      TestCase e -> specItem "<unlabeled>" e
