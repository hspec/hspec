module Test.Hspec.Core.Config.DefinitionSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Config.Definition

spec :: Spec
spec = do
  describe "formatOrList" $ do
    it "formats a list of or-options" $ do
      formatOrList ["foo", "bar", "baz"] `shouldBe` "foo, bar or baz"
