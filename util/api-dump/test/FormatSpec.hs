module FormatSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List

import           Format

spec :: Spec
spec = do
  describe "formatSum" $ do
    it "formats a sum type" $ do
      let input = "data Foo = Foo | Bar | Baz"
      formatSum input `shouldBe` intercalate "\n" [
          "data Foo"
        , "  | Foo"
        , "  | Bar"
        , "  | Baz"
        ]

  describe "formatRecord" $ do
    it "formats a record type" $ do
      let input = "data Person = Person {personName :: String, personAge :: Int}"
      formatRecord input `shouldBe` intercalate "\n" [
          "data Person = Person"
        , "  personName :: String"
        , "  personAge :: Int"
        ]

    it "correctly handles commas from nested types" $ do
      let input = "newtype Tuple a b = Tuple {unTuple :: (a, b)}"
      formatRecord input `shouldBe` intercalate "\n" [
          "newtype Tuple a b = Tuple"
        , "  unTuple :: (a, b)"
        ]

  describe "joinDefinitions" $ do
    it "preserves newlines in class definitions" $ do
      let
        input = [
            "class Foo a where"
          , "  type Arg :: * -> *"
          , "  type family Arg e"
          , "    Default: ()"
          , "  foo :: a -> Int -> String"
          , "  {-# MINIMAL foo #-}"
          ]
      joinDefinitions input `shouldBe` [ intercalate "\n" input ]

    it "joins line continuations in class definitions" $ do
      let
        input = [
            "class Foo a where"
          , "  foo :: a"
          , "      -> Int"
          , "      -> String"
          ]
      joinDefinitions input `shouldBe` [intercalate "\n" [
          "class Foo a where"
        , "  foo :: a -> Int -> String"
        ]]

  describe "breakOutsideParens" $ do
    it "breaks a string" $ do
      breakOutsideParens (== '{') "foo { ... }" `shouldBe` ("foo ", "{ ... }")

    it "skips over parentheses" $ do
      breakOutsideParens (== '{') "foo ((foo, bar), { ... }, baz) { ..." `shouldBe` ("foo ((foo, bar), { ... }, baz) ", "{ ...")

    context "when a string does not contain any opening parenthesis" $ do
      it "is equivalent to `break`" $ do
        property $ \ (Fun _ p) xs -> '(' `notElem` xs ==> do
          breakOutsideParens p xs `shouldBe` break p xs

  describe "skipParens" $ do
    let
      test :: HasCallStack => (String, String) -> Expectation
      test expected@(xs, ys) = skipParens (xs <> ys) `shouldBe` expected

    it "breaks a string after a closing parenthesis" $ do
      test ("...)", " bar")

    it "skips over parenthesized terms" $ do
      test ("foo ( bar ) )", " baz")

    it "skips over nested parenthesized terms" $ do
      test ("foo ( ( ( ( bar ) ) ) ) ( test) )", " baz")

    it "does not discard or change any characters" $ do
      property $ \ xs -> do
        let (ys, zs) = skipParens xs
        ys ++ zs `shouldBe` xs
