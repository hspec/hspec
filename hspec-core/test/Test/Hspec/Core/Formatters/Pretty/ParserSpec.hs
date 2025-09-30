{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.Core.Formatters.Pretty.ParserSpec (
  spec
, Person(..)
, Address(..)
, person
) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.Parser

data OperatorType = String :*: Int
  deriving (Eq, Show)

data Person = Person {
  personName :: String
, personAge :: Int
, personAddress :: Maybe Address
} deriving (Eq, Show)

data Address = Address {
  addressStreet :: String
, addressPostalCode :: Int
} deriving (Eq, Show)

person :: Person
person = Person "Joe" 23 (Just $ Address "Main Street" 50000)

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
      show (0.5 :: Rational) `shouldParseAs` Operator (Number "1") "%" (Number "2")

    it "accepts negative rationals" $ do
      show (-0.5 :: Rational) `shouldParseAs` Operator (parentheses $ Number "-1") "%" (Number "2")

    it "accepts constructor symbols" $ do
      show ("foo" :*: 23) `shouldParseAs` Operator (String "foo") ":*:" (Number "23")

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
      show person `shouldParseAs` Record "Person" [
          ("personName", String "Joe")
        , ("personAge", Number "23")
        , ("personAddress", Constructor "Just" [Tuple [Record "Address" [
            ("addressStreet", String "Main Street")
          , ("addressPostalCode", Number "50000")
          ]]])
        ]

    context "with deeply nested data structures" $ do
      it "completes in O(n) time" $ do
        let
          input = show (
            "0", (
            "1", (
            "2", (
            "3", (
            "4", (
            "5", (
            "6", (
            "7", (
            "8", (
            "9", (
            "10", (
            "11", (
            "12", (
            "13", (
            "14", (
            "15", (
            "16", (
            "17", (
            "18", (
            "19", (
            "20", (
            "21", (
            "22", (
            "23", (
            "24", (
            "25", (
            "26", (
            "27", (
            "28", (
            "29", (
            )))))))))))))))))))))))))))))))
        r <- timeout 1 $ evaluate (parseValue input)
        (join r :: Maybe Value) `shouldSatisfy` isJust
