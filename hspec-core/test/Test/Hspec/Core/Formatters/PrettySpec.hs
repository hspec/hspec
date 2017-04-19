module Test.Hspec.Core.Formatters.PrettySpec (spec) where

import           Prelude ()
import           Data.List
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.ParserSpec (User(..))

import           Test.Hspec.Core.Formatters.Pretty

spec :: Spec
spec = do
  let user = show $ User "John" "Doe" 23

  describe "pretty" $ do
    it "pretty-prints lists" $ do
      let xs = [1, 2, 3 :: Int]
      pretty (show xs) `shouldBe` intercalate "\n" [
          "["
        , "  1,"
        , "  2,"
        , "  3"
        , "]"
        ]

    it "pretty-prints records" $ do
      pretty user `shouldBe` intercalate "\n" [
          "User {"
        , "  firstName = \"John\","
        , "  lastName = \"Doe\","
        , "  age = 23"
        , "}"
        ]

  describe "pretty2" $ do
    it "pretty-prints two values" $ do
      pretty2 user user `shouldBe` (pretty user, pretty user)

    context "when one of the values can not be pretty-printed" $ do
      it "it does not pretty-print the other one neither" $ do
        pretty2 user "foo" `shouldBe` (user, "foo")
