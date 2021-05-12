module Foo.BazSpec (spec) where

import           Test.Hspec

spec :: SpecWith String
spec = do
  it "should be 23" (`shouldBe` "23")
