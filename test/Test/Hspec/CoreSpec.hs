module Test.Hspec.CoreSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Util
import qualified Test.Hspec as H

main :: IO ()
main = hspecX spec

spec :: Spec
spec = do
  describe "Bool as an example" $ do
    it "succeds, when True" $ do
      hspecSummary [H.it "foo" True] `shouldReturn` H.Summary 1 0

    it "fails, when False" $ do
      hspecSummary [H.it "foo" False] `shouldReturn` H.Summary 1 1

    it "fails, when undefined" $ do
      hspecSummary [H.it "foo" (undefined :: Bool)] `shouldReturn` H.Summary 1 1
