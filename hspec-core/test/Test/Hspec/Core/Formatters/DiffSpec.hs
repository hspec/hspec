{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Formatters.DiffSpec (spec) where

import           Prelude ()
import           Helper hiding (First)
import           Data.Char

import           Test.Hspec.Core.Formatters.Diff as Diff

dropQuotes :: String -> String
dropQuotes = init . tail

spec :: Spec
spec = do
  describe "recover" $ do
    context "with single-line string literals" $ do
      context "with --unicode" $ do
        it "recovers unicode" $ do
          recover True (show "foo\955bar") (show "foo-bar") `shouldBe` ("\"foo\955bar\"", "\"foo-bar\"")

      context "with --no-unicode" $ do
        it "does not recover unicode" $ do
          recover False (show "foo\955bar") (show "foo-bar") `shouldBe` ("\"foo\\955bar\"", "\"foo-bar\"")

  describe "recoverString" $ do
    it "parses back multi-line string literals" $ do
      recoverString True (show "foo\nbar\nbaz\n") `shouldBe` Just "foo\nbar\nbaz\n"

    it "does not parse back string literals that contain control characters" $ do
      recoverString True (show "foo\n\tbar\nbaz\n") `shouldBe` Nothing

    it "does not parse back string literals that span a single line" $ do
      recoverString True (show "foo\n") `shouldBe` Nothing

    context "when unicode is True" $ do
      it "parses back string literals that contain unicode" $ do
        recoverString True (show "foo\n\955\nbaz\n") `shouldBe` Just "foo\n\955\nbaz\n"

    context "when unicode is False" $ do
      it "does not parse back string literals that contain unicode" $ do
        recoverString False (show "foo\n\955\nbaz\n") `shouldBe` Nothing

  describe "partition" $ do
    context "with a single shown Char" $ do
      it "never partitions a character escape" $ do
        property $ \ (c :: Char) -> partition (show c) `shouldBe` ["'", dropQuotes (show c), "'"]

    context "with a shown String" $ do
      it "puts backslash-escaped characters into separate chunks" $ do
        partition (show "foo\nbar") `shouldBe` ["\"", "foo", "\\n", "bar", "\""]

      it "puts *arbitrary* backslash-escaped characters into separate chunks" $ do
        property $ \ xs c ys ->
          let
            char = dropQuotes (show [c])
            isEscaped = length char > 1
            escape = tail char
            sep = case ys of
              x : _ | all isDigit escape && isDigit x || escape == "SO" && x == 'H' -> ["\\&"]
              _ -> []
            actual = partition (show (xs ++ c : ys))
            expected = partition (init $ show xs) ++ [char] ++ sep ++ partition (tail $ show ys)
          in isEscaped ==> actual `shouldBe` expected

  describe "breakList" $ do
    context "with a list where the predicate matches at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "foo bar  baz" `shouldBe` ["foo", " ", "bar", " ", " ", "baz"]

    context "with a list where the predicate does not match at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "  foo bar  baz  " `shouldBe` [" ", " ", "foo", " ", "bar", " ", " ", "baz", " ", " "]
