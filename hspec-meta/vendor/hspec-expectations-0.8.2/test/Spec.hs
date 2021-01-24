module Main where

import Test.Hspec

import qualified Test.Hspec.ExpectationsSpec
import qualified Test.Hspec.Expectations.MatcherSpec

spec :: Spec
spec = do
  describe "Test.Hspec.ExpectationsSpec" Test.Hspec.ExpectationsSpec.spec
  describe "Test.Hspec.Expectations.MatcherSpec" Test.Hspec.Expectations.MatcherSpec.spec

main :: IO ()
main = hspec spec
