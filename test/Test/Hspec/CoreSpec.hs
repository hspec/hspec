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

  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec [H.it "foo" H.pending]
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

    it "accepts an optional message, which is included in the report" $ do
      r <- runSpec [H.it "foo" $ H.pending "for some reason"]
      r `shouldSatisfy` any (== "     # PENDING: for some reason")
