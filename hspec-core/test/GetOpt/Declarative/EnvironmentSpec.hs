{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module GetOpt.Declarative.EnvironmentSpec (spec) where

import           Prelude ()
import           Helper

import           GetOpt.Declarative.Types
import           GetOpt.Declarative.Environment

spec :: Spec
spec = do
  describe "parseEnvironmentOption" $ do
    context "with NoArg" $ do
      let
        option :: Option Bool
        option = Option {
          optionName = "some-flag"
        , optionSetter = NoArg $ const True
        }
      it "accepts 'yes'" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "yes")] False option `shouldBe` Right True

      it "rejects other values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "no")] False option `shouldBe` invalidValue "FOO_SOME_FLAG" "no"

    context "with Flag" $ do
      let
        option :: Option Bool
        option = Option {
          optionName = "some-flag"
        , optionSetter = Flag $ \ value _ -> value
        }
      it "accepts 'yes'" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "yes")] False option `shouldBe` Right True

      it "accepts 'no'" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "no")] True option `shouldBe` Right False

      it "rejects other values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "nay")] True option `shouldBe` invalidValue "FOO_SOME_FLAG" "nay"

    context "with OptArg" $ do
      let
        option :: Option String
        option = Option {
          optionName = "some-flag"
        , optionSetter = OptArg undefined $ \ (Just arg) _ -> guard (arg == "yes") >> Just arg
        }

      it "accepts valid values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "yes")] "" option `shouldBe` Right "yes"

      it "rejects invalid values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "no")] "" option `shouldBe` invalidValue "FOO_SOME_FLAG" "no"

    context "with Arg" $ do
      let
        option :: Option String
        option = Option {
          optionName = "some-flag"
        , optionSetter = Arg undefined $ \ arg _ -> guard (arg == "yes") >> Just arg
        }

      it "accepts valid values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "yes")] "" option `shouldBe` Right "yes"

      it "rejects invalid values" $ do
        parseEnvironmentOption "FOO" [("FOO_SOME_FLAG", "no")] "" option `shouldBe` invalidValue "FOO_SOME_FLAG" "no"
  where
    invalidValue name = Left . InvalidValue name
