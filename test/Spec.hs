module Main (main) where

import           Test.Hspec.ShouldBe

import qualified Test.Hspec.MonadicSpec

main :: IO ()
main = hspecX $ do
  describe "Test.Hspec.Monadic" Test.Hspec.MonadicSpec.spec
