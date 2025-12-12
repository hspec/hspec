{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Formatters.DiffSpec (spec) where

import           Prelude ()
import           Helper
import           Data.Char

import           Test.Hspec.Core.Formatters.Diff as Diff

dropQuotes :: String -> String
dropQuotes = init . drop 1

spec :: Spec
spec = do
  describe "lineDiff" $ do
    let
      enumerate name n = map ((name ++) . show) [1 .. n :: Int]
      diff_ expected actual = lineDiff (Just 2) (unlines expected) (unlines actual)

    it "suppresses excessive diff output" $ do
      let
        expected = enumerate "foo" 99
        actual = replace "foo50" "bar50" expected

      diff_ expected actual `shouldBe` [
          LinesOmitted 47
        , LinesBoth [
            "foo48"
          , "foo49"
          ]
        , SingleLineDiff [First "foo50", Second "bar50"]
        , LinesBoth [
            "foo51"
          , "foo52"
          ]
        , LinesOmitted 47
        , LinesBoth [""]
        ]

    it "ensures that omitted sections are at least three lines in size" $ do
      forAll (elements [1..20]) $ \ size -> do
        let expected = enumerate "" size
        forAll (elements expected) $ \ i -> do
          let actual = replace i "bar" expected
          [n | LinesOmitted n <- diff_ expected actual] `shouldSatisfy` all (>= 3)

    context "with modifications within a line" $ do
        it "suppresses excessive diff output" $ do
          let
            expected = enumerate "foo " 99
            actual = replace "foo 42" "foo 23" expected

          diff_ expected actual `shouldBe` [
              LinesOmitted 39
            , LinesBoth [
                "foo 40"
              , "foo 41"
              ]
            , SingleLineDiff  [Both "foo ", First "42", Second "23"]
            , LinesBoth [
                "foo 43"
              , "foo 44"
              ]
            , LinesOmitted 55
            , LinesBoth [""]
            ]

    context "with modifications at start / end" $ do
      it "suppresses excessive diff output" $ do
        let
          expected = enumerate "foo" 9
          actual = replace "foo9" "bar9" $ replace "foo1" "bar1" expected

        diff_ expected actual `shouldBe` [
            SingleLineDiff  [First "foo1", Second "bar1"]
          , LinesBoth [
              "foo2"
            , "foo3"
            ]
          , LinesOmitted 3
          , LinesBoth [
              "foo7"
            , "foo8"
            ]
          , SingleLineDiff  [First "foo9", Second "bar9"]
          , LinesBoth [""]
          ]

  describe "splitLines" $ do
    it "splits on newline characters" $ do
      splitLines "foo\nbar\nbaz" `shouldBe` ["foo", "bar", "baz"]

    describe "with a newline characters at the start" $ do
      it "splits on newline characters" $ do
        splitLines "\nfoo\nbar\nbaz" `shouldBe` ["", "foo", "bar", "baz"]

    describe "with a newline characters at the end" $ do
      it "splits on newline characters" $ do
        splitLines "foo\nbar\nbaz\n" `shouldBe` ["foo", "bar", "baz", ""]

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
            escape = drop 1 char
            sep = case ys of
              x : _ | all isDigit escape && isDigit x || escape == "SO" && x == 'H' -> ["\\&"]
              _ -> []
            actual = partition (show (xs ++ c : ys))
            expected = partition (init $ show xs) ++ [char] ++ sep ++ partition (drop 1 $ show ys)
          in isEscaped ==> actual `shouldBe` expected

  describe "breakList" $ do
    context "with a list where the predicate matches at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "foo bar  baz" `shouldBe` ["foo", " ", "bar", " ", " ", "baz"]

    context "with a list where the predicate does not match at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "  foo bar  baz  " `shouldBe` [" ", " ", "foo", " ", "bar", " ", " ", "baz", " ", " "]
