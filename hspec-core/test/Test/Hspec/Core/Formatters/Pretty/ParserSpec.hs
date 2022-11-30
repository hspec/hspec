{-# LANGUAGE CPP #-}
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
shouldParseAs input expected = unsafeParseValue input `shouldBe` just expected
  where
#if __GLASGOW_HASKELL__ >= 802
    just = Just
#else
    just _ = Nothing
#endif

spec :: Spec
spec = do
  describe "parseValue" $ do
    it "parses characters" $ do
      show 'c' `shouldParseAs` Char 'c'

    it "parses strings" $ do
      show "foo" `shouldParseAs` String "foo"

    it "parses integers" $ do
      "23" `shouldParseAs` Integer 23

    it "parses negative integers" $ do
      "-23" `shouldParseAs` Integer (-23)

    it "parses rationals" $ do
      "23.0" `shouldParseAs` Rational "23.0"

    it "parses negative rationals" $ do
      "-23.0" `shouldParseAs` Rational "-23.0"

    it "parses lists" $ do
      show ["foo", "bar", "baz"] `shouldParseAs` List [String "foo", String "bar", String "baz"]

    it "parses tuples" $ do
      show ("foo", "bar", "baz") `shouldParseAs` Tuple [String "foo", String "bar", String "baz"]

    it "parses Nothing" $ do
      show (Nothing :: Maybe Int) `shouldParseAs` Id "Nothing"

    it "parses Just" $ do
      show (Just "foo") `shouldParseAs` App (Id "Just") (String "foo")

    it "parses nested Just" $ do
      show (Just $ Just "foo") `shouldParseAs` App (Id "Just") (Parentheses $ App (Id "Just") (String "foo"))

    it "parses records" $ do
      let person = Person "Joe" 23

      show person `shouldParseAs` Record "Person" [
          ("personName", String "Joe")
        , ("personAge", Integer 23)
        ]
