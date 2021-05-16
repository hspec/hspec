module GetOpt.Declarative.UtilSpec (spec) where

import           Prelude ()
import           Helper

import           System.Console.GetOpt

import           GetOpt.Declarative.Util

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
