module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           System.Exit
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as H
import           Test.Hspec.Formatters
import           System.IO
import           System.IO.Silently

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hspec" $ do
    it "returns (), if all examples pass" $ do
      H.hspec [H.it "foobar" True] `shouldReturn` ()

    it "exits with exitFailure, if not all examples pass" $ do
      H.hspec [H.it "foobar" False] `shouldThrow` (== ExitFailure 1)

  describe "the \"hHspecWithFormat\" function" $ do
    let testSpecs = [
          H.describe "Example" [
            H.it "success"   True
          , H.it "fail 1"    False
          , H.it "pending"   H.pending
          , H.it "fail 2"    False
          , H.it "exception" (undefined :: Bool)
          , H.it "fail 3"    False
          ]
          ]
        runSpec f = (lines . fst) `fmap` capture (H.hHspecWithFormat f False stdout testSpecs)

    it "can use the \"silent\" formatter to show no output" $ do
      runSpec silent `shouldReturn` []

    it "can use the \"progress\" formatter to show '..F...FF.F' style output" $ do
      r <- runSpec progress
      head r `shouldBe` ".F.FFF"

    it "can use the \"specdoc\" formatter to show all examples (default)" $ do
      r <- runSpec specdoc
      r !! 1 `shouldBe` "Example"

    it "can use the \"failed_examples\" formatter to show only failed examples" $ do
      r <- runSpec failed_examples
      r !! 1 `shouldBe` "1) Example fail 1 FAILED"

  describe "hHspecWithFormat" $ do
    it "returns a summary of the test run" $ do
      H.hHspecWithFormat silent False stdout [
          H.it "foo" True
        , H.it "foo" False
        , H.it "foo" False
        , H.it "foo" True
        , H.it "foo" True
        ] `shouldReturn` H.Summary 5 2
