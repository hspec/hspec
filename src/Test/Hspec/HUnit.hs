{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Importing this module allows you to use an @HUnit@ `HU.Test` as an example
-- for a behavior.  You can use an explicit `HU.TestCase` data constructor or
-- use an `HU.Assertion`.  For an @Assertion@, any exception means the example
-- failed; otherwise, it's successfull.
--
-- NOTE: Any output from the example to @stdout@ is ignored.  If you need to
-- write out for debugging, you can write to @stderr@ or a file handle.
--
-- > import Test.Hspec.Monadic
-- > import Test.Hspec.HUnit ()
-- > import Test.HUnit
-- >
-- > main :: IO ()
-- > main = hspec $ do
-- >   describe "reverse" $ do
-- >     it "reverses a list" $ do
-- >       reverse [1, 2, 3] @?= [3, 2, 1]
-- >
-- >     it "gives the original list, if applied twice" $ TestCase $
-- >       (reverse . reverse) [1, 2, 3] @?= [1, 2, 3]
--
module Test.Hspec.HUnit (fromHUnitTest) where

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

fromHUnitTest :: Test -> [SpecTree]
fromHUnitTest t = case t of
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
