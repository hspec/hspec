{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.ResultSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Format

import           Test.Hspec.Core.Runner.Result

spec :: Spec
spec = do
  describe "Summary" $ do
    let
      summary :: Summary
      summary = toSummary $ toSpecResult [item Success, item failure]

    it "can be deconstructed via accessor functions" $ do
      (summaryExamples &&& summaryFailures) summary `shouldBe` (2, 1)

    it "can be deconstructed via pattern matching" $ do
      let Summary examples failures = summary
      (examples, failures) `shouldBe` (2, 1)

    it "can be deconstructed via RecordWildCards" $ do
      let Summary{..} = summary
      (summaryExamples, summaryFailures) `shouldBe` (2, 1)

  describe "specResultSuccess" $ do
    context "when all spec items passed" $ do
      it "returns True" $ do
        specResultSuccess (toSpecResult [item Success]) `shouldBe` True

    context "with a failed spec item" $ do
      it "returns False" $ do
        specResultSuccess (toSpecResult [item Success, item failure]) `shouldBe` False

    context "with an empty result list" $ do
      it "returns True" $ do
        specResultSuccess (toSpecResult []) `shouldBe` True
  where
    failure = Failure Nothing NoReason
    item result = (([], ""), Item Nothing 0 "" result)
