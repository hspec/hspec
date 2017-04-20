module Test.Hspec.Core.Config.UtilSpec (spec) where

import           Helper

import           Test.Hspec.Core.Config.Util

spec :: Spec
spec = do
  describe "formatOrList" $ do
    it "" $ do
      formatOrList ["foo", "bar", "baz"] `shouldBe` "foo, bar or baz"

    it "" $ do
      formatOrList ["foo"] `shouldBe` "foo"
