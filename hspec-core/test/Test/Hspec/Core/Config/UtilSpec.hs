module Test.Hspec.Core.Config.UtilSpec (spec) where

import           Helper

import           System.Console.GetOpt

import           Test.Hspec.Core.Config.Util

spec :: Spec
spec = do
  describe "mkUsageInfo" $ do
    it "restricts output size to 80 characters" $ do
      let options = [
              Option "" ["color"] (NoArg ()) (unwords $ replicate 3 "some very long and verbose help text")
            ]
      mkUsageInfo "" options `shouldBe` unlines [
          ""
        , "    --color  some very long and verbose help text some very long and verbose"
        , "             help text some very long and verbose help text"
        ]

    it "condenses help for --no-options" $ do
      let options = [
              Option "" ["color"] (NoArg ()) "some help"
            , Option "" ["no-color"] (NoArg ()) "some other help"
            ]
      mkUsageInfo "" options `shouldBe` unlines [
          ""
        , "    --[no-]color  some help"
        ]

  describe "formatOrList" $ do
    it "formats a list of or-options" $ do
      formatOrList ["foo", "bar", "baz"] `shouldBe` "foo, bar or baz"
