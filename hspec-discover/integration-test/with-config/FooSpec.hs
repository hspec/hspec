module FooSpec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "foo" $ do
    it "reverses a list" False
