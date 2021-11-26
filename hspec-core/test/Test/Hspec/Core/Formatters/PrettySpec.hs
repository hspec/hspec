{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Formatters.PrettySpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.ParserSpec (Person(..))

import           Test.Hspec.Core.Formatters.Pretty

spec :: Spec
spec = do
  describe "pretty2" $ do
    context "with single-line string literals" $ do
      context "with --unicode" $ do
        it "recovers unicode" $ do
          pretty2 True (show "foo\955bar") (show "foo-bar") `shouldBe` ("\"foo\955bar\"", "\"foo-bar\"")

      context "with --no-unicode" $ do
        it "does not recover unicode" $ do
          pretty2 False (show "foo\955bar") (show "foo-bar") `shouldBe` ("\"foo\\955bar\"", "\"foo-bar\"")

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

  describe "pretty" $ do
    let person = Person "Joe" 23

    it "pretty-prints records" $ do
      pretty True (show person) `shouldBe` just [
          "Person {"
        , "  personName = \"Joe\","
        , "  personAge = 23"
        , "}"
        ]

    it "pretty-prints Just-values" $ do
      pretty True (show $ Just person) `shouldBe` just [
          "Just Person {"
        , "  personName = \"Joe\","
        , "  personAge = 23"
        , "}"
        ]

    it "pretty-prints tuples" $ do
      pretty True (show (person, 23 :: Int)) `shouldBe` just [
          "(Person {"
        , "  personName = \"Joe\","
        , "  personAge = 23"
        , "}, 23)"
        ]

    it "pretty-prints lists" $ do
      pretty True (show [Just person, Nothing]) `shouldBe` just [
          "[Just Person {"
        , "  personName = \"Joe\","
        , "  personAge = 23"
        , "}, Nothing]"
        ]

    context "with --unicode" $ do
      it "retains unicode characters in record fields" $ do
        pretty True (show $ Person "λ-Joe" 23) `shouldBe` just [
            "Person {"
          , "  personName = \"λ-Joe\","
          , "  personAge = 23"
          , "}"
          ]

      it "retains unicode characters in list elements" $ do
        pretty True (show ["foo", "λ", "bar"]) `shouldBe` just ["[\"foo\", \"λ\", \"bar\"]"]

    context "with --no-unicode" $ do
      it "does not retain unicode characters in record fields" $ do
        pretty False (show $ Person "λ-Joe" 23) `shouldBe` just [
            "Person {"
          , "  personName = \"\\955-Joe\","
          , "  personAge = 23"
          , "}"
          ]

      it "does not retain unicode characters in list elements" $ do
        pretty False (show ["foo", "λ", "bar"]) `shouldBe` just ["[\"foo\", \"\\955\", \"bar\"]"]

    context "with input that looks like a list" $ do
      it "it returns Nothing" $ do
        pretty True "[23,42]" `shouldBe` Nothing

    context "with input that looks like a tuple" $ do
      it "it returns Nothing" $ do
        pretty True "(23,42)" `shouldBe` Nothing

    context "with input that looks like function applications" $ do
      it "it returns Nothing" $ do
        let input = unlines ["foo", "bar", "baz"]
        pretty True input `shouldBe` Nothing
  where
#if __GLASGOW_HASKELL__ >= 802
    just = Just . intercalate "\n"
#else
    just _ = Nothing
#endif
