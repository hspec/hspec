{-# OPTIONS -fno-warn-orphans #-}
module Test.Hspec.HUnit (
-- * Interoperability with HUnit
  fromHUnitTest
) where

import           Data.List (intersperse)
import qualified Test.HUnit as HU
import           Test.HUnit (Test (..))

import           Test.Hspec.Internal

-- |
-- This instance is deprecated and will be removed in a future release!  Use
-- `Test.Hspec.HUnit.fromHUnitTest` instead!
instance Example Test where
  evaluateExample _ test = do
    (counts, fails) <- HU.runTestText HU.putTextToShowS test
    let r = if HU.errors counts + HU.failures counts == 0
             then Success
             else Fail (details $ fails "")
    return r
    where
      details :: String -> String
      details = concat . intersperse "\n" . tail . init . lines

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
      TestLabel s (TestCase e)  -> SpecExample s (`evaluateExample` e)
      TestLabel s (TestList xs) -> SpecGroup s (map go xs)
      TestLabel s x             -> SpecGroup s [go x]
      TestList xs               -> SpecGroup   "<unlabeled>" (map go xs)
      TestCase e                -> SpecExample "<unlabeled>" (`evaluateExample` e)
