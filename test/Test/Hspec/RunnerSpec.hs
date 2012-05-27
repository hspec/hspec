module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           System.Exit
import qualified Test.Hspec.Runner as H

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "hspec" $ do
    it "returns (), if all examples pass" $ do
      H.hspec [H.it "foobar" True] `shouldReturn` ()

    it "exits with exitFailure, if not all examples pass" $ do
      H.hspec [H.it "foobar" False] `shouldThrow` (== ExitFailure 1)
