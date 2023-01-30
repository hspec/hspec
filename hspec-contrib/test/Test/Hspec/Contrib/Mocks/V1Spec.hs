module Test.Hspec.Contrib.Mocks.V1Spec (spec) where

import           Test.Hspec
import           Test.HUnit.Lang

import           Test.Hspec.Contrib.Mocks.V1

hUnitFailure :: FailureReason -> HUnitFailure -> Bool
hUnitFailure expected (HUnitFailure _ actual) = actual == expected

spec :: Spec
spec = do
  describe "stubAction" $ do
    it "creates a stub action" $ do
      stub <- stubAction [23, 42, 65 :: Int]
      stub `shouldReturn` 23
      stub `shouldReturn` 42
      stub `shouldReturn` 65
      stub `shouldThrow` hUnitFailure (Reason "stubAction: no values left")

  describe "withSpy" $ do
    it "records arguments" $ do
      withSpy $ \ spy -> do
        spy "foo"
        spy "bar"
        spy "baz"
      `shouldReturn` ["foo", "bar", "baz"]
