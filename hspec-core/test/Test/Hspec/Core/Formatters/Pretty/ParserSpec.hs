{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Formatters.Pretty.ParserSpec (spec, Person(..)) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.Parser

data Person = Person {
  personName :: String
, personAge :: Int
} deriving (Eq, Show)

string :: String -> Expression
string = Literal . String

integer :: Integer -> Expression
integer = Literal . Integer

spec :: Spec
spec = do
  let parse = unsafeParseExpression
  describe "parseExpression" $ do
    it "parses characters" $ do
      parse (show 'c') `shouldBe` just (Literal $ Char 'c')

    it "parses strings" $ do
      parse (show "foo") `shouldBe` just (string "foo")

    it "parses integers" $ do
      parse "23" `shouldBe` just (integer 23)

    it "parses negative integers" $ do
      parse "-23" `shouldBe` just (integer (-23))

    it "parses rationals" $ do
      parse "23.0" `shouldBe` just (Literal $ Rational "23.0")

    it "parses negative rationals" $ do
      parse "-23.0" `shouldBe` just (Literal $ Rational "-23.0")

    it "parses lists" $ do
      parse (show ["foo", "bar", "baz"]) `shouldBe` just (List [string "foo", string "bar", string "baz"])

    it "parses tuples" $ do
      parse (show ("foo", "bar", "baz")) `shouldBe` just (Tuple [string "foo", string "bar", string "baz"])

    it "parses Nothing" $ do
      parse (show (Nothing :: Maybe Int)) `shouldBe` just (Id "Nothing")

    it "parses Just" $ do
      parse (show $ Just "foo") `shouldBe` just (App (Id "Just") (string "foo"))

    it "parses nested Just" $ do
      parse (show $ Just $ Just "foo") `shouldBe` just (App (Id "Just") . Parentheses $ App (Id "Just") (string "foo"))

    it "parses records" $ do
      let person = Person "Joe" 23

      parse (show person) `shouldBe` just (Record "Person" [
          ("personName", string "Joe")
        , ("personAge", integer 23)
        ])
  where
#if __GLASGOW_HASKELL__ >= 802
    just = Just
#else
    just _ = Nothing
#endif
