module Test.Hspec.Core.Config.UtilSpec (spec) where

import           Helper

import           System.Console.GetOpt

import           Test.Hspec.Core.Config.Util

spec :: Spec
spec = do
  describe "condenseNoOptions" $ do
    it "condenses help for --no-options" $ do
      let options = [
              Option "" ["color"] (NoArg ()) "some help"
            , Option "" ["no-color"] (NoArg ()) "some other help"
            ]
      usageInfo "" (condenseNoOptions options) `shouldBe` unlines [
          ""
        , "    --[no-]color  some help"
        ]

  describe "formatOrList" $ do
    it "formats a list of or-options" $ do
      formatOrList ["foo", "bar", "baz"] `shouldBe` "foo, bar or baz"
