module FooSpec (spec) where

import           Test.Hspec

spec :: SpecWith Int
spec = do
  it "should be 23" (`shouldBe` 23)
