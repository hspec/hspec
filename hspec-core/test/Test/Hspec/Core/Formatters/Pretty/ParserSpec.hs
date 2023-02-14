{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.Core.Formatters.Pretty.ParserSpec (spec, Person(..)) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.Parser

data Person = Person {
  personName :: String
, personAge :: Int
} deriving (Eq, Show)

infix 1 `shouldParseAs`

shouldParseAs :: HasCallStack => String -> Value -> Expectation
shouldParseAs input expected = parseValue input `shouldBe` Just expected

unit :: Value
unit = Tuple []

parentheses :: Value -> Value
parentheses value = Tuple [value]

spec :: Spec
spec = do
  describe "parseValue" $ do
    it "parses unit" $ do
      show () `shouldParseAs` unit

    it "parses characters" $ do
      show 'c' `shouldParseAs` Char 'c'

    it "parses strings" $ do
      show "foo" `shouldParseAs` String "foo"

    it "accepts rationals" $ do
      show (0.5 :: Rational) `shouldParseAs` Rational (Number "1") (Number "2")

    it "accepts negative rationals" $ do
      show (-0.5 :: Rational) `shouldParseAs` Rational (parentheses $ Number "-1") (Number "2")

    it "accepts integers" $ do
      "23" `shouldParseAs` Number "23"

    it "accepts negative integers" $ do
      "-23" `shouldParseAs` Number "-23"

    it "accepts floats" $ do
      show (23.0 :: Float) `shouldParseAs` Number "23.0"

    it "accepts negative floats" $ do
      show (-23.0 :: Float) `shouldParseAs` Number "-23.0"

    it "parses lists" $ do
      show ["foo", "bar", "baz"] `shouldParseAs` List [String "foo", String "bar", String "baz"]

    it "parses tuples" $ do
      show ("foo", "bar", "baz") `shouldParseAs` Tuple [String "foo", String "bar", String "baz"]

    it "parses Nothing" $ do
      show (Nothing :: Maybe Int) `shouldParseAs` Constructor "Nothing" []

    it "parses Just" $ do
      show (Just "foo") `shouldParseAs` Constructor "Just" [String "foo"]

    it "parses nested Just" $ do
      show (Just $ Just "foo") `shouldParseAs` Constructor "Just" [parentheses (Constructor "Just" [String "foo"])]

    it "parses records" $ do
      let person = Person "Joe" 23

      show person `shouldParseAs` Record "Person" [
          ("personName", String "Joe")
        , ("personAge", Number "23")
        ]
