module Test.Hspec.Core.ClockSpec (spec) where

import           Helper

import           Test.Hspec.Core.Clock

spec :: Spec
spec = do
  describe "toMicroseconds" $ do
    it "converts Seconds to microseconds" $ do
      toMicroseconds 2.5 `shouldBe` 2500000
