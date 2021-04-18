module Test.Hspec.Core.ClockSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Clock

spec :: Spec
spec = do
  describe "toMilliseconds" $ do
    it "converts Seconds to milliseconds" $ do
      toMilliseconds 0.1 `shouldBe` 100

  describe "toMicroseconds" $ do
    it "converts Seconds to microseconds" $ do
      toMicroseconds 2.5 `shouldBe` 2500000
