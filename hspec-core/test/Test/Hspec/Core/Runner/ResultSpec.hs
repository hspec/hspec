module Test.Hspec.Core.Runner.ResultSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Format

import           Test.Hspec.Core.Runner.Result

spec :: Spec
spec = do
  describe "specResultSuccess" $ do
    let
      failure = Failure Nothing NoReason
      item result = (([], ""), Item Nothing 0 "" result)

    context "when all spec items passed" $ do
      it "returns True" $ do
        specResultSuccess (toSpecResult False [item Success]) `shouldBe` True

    context "with a failed spec item" $ do
      it "returns False" $ do
        specResultSuccess (toSpecResult False [item Success, item failure]) `shouldBe` False

    context "with an empty result list" $ do
      it "returns True" $ do
        specResultSuccess (toSpecResult False []) `shouldBe` True

    context "when configFailOnEmpty is True" $ do
      context "when all spec items passed" $ do
        it "returns True" $ do
          specResultSuccess (toSpecResult True [item Success]) `shouldBe` True

      context "with an empty result list" $ do
        it "returns False" $ do
          specResultSuccess (toSpecResult True []) `shouldBe` False
