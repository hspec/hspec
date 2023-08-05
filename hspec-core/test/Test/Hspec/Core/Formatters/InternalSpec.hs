{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Formatters.InternalSpec (spec) where

import           Prelude ()
import           Helper

import           System.Console.ANSI

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Formatters.Internal

formatConfig :: FormatConfig
formatConfig = defaultFormatConfig {
  formatConfigUseColor = True
, formatConfigUseDiff = True
, formatConfigDiffContext = Just 3
}

spec :: Spec
spec = do
  forM_ [
      ("extraChunk", extraChunk, Red)
    , ("missingChunk", missingChunk, Green)
    ] $ \ (name, chunk, color) -> do

    describe name $ do
      it "colorizes chunks" $ do
        capture_ $ runFormatM formatConfig $ do
          chunk "foo"
        `shouldReturn` colorize Foreground color "foo"

      context "with an all-spaces chunk" $ do
        it "colorizes background" $ do
          capture_ $ runFormatM formatConfig $ do
            chunk "  "
          `shouldReturn` colorize Background color "  "

      context "with an all-newlines chunk" $ do
        it "colorizes background" $ do
          capture_ $ runFormatM formatConfig $ do
            chunk "\n\n\n"
          `shouldReturn` colorize Background color "\n\n\n"

  describe "write" $ do
    it "does not span colored output over multiple lines" $ do

      -- This helps with output on Jenkins and Buildkite:
      -- https://github.com/hspec/hspec/issues/346

      capture_ $ runFormatM formatConfig $ do
        withSuccessColor $ write "foo\nbar\nbaz\n"
      `shouldReturn` unlines [green "foo", green "bar", green "baz"]

  describe "splitLines" $ do
    it "splits a string into chunks" $ do
      splitLines "foo\nbar\nbaz" `shouldBe` ["foo", "\n", "bar", "\n", "baz"]

    it "splits *arbitrary* strings into chunks" $ do
      property $ \ xs -> do
        mconcat (splitLines xs) `shouldBe` xs

    it "puts newlines into separate chunks" $ do
      property $ \ xs -> do
        filter (notElem '\n') (splitLines xs) `shouldBe` filter (not . null) (lines xs)
