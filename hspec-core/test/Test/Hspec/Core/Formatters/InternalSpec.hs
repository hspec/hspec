module Test.Hspec.Core.Formatters.InternalSpec (spec) where

import           Prelude ()
import           Helper

import           System.Console.ANSI

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Formatters.Internal

formatConfig :: FormatConfig
formatConfig = FormatConfig {
  formatConfigUseColor = True
, formatConfigReportProgress = False
, formatConfigOutputUnicode = False
, formatConfigUseDiff = True
, formatConfigDiffContext = Just 3
, formatConfigPrettyPrint = False
, formatConfigPrettyPrintFunction = Nothing
, formatConfigPrintTimes = False
, formatConfigHtmlOutput = False
, formatConfigPrintCpuTime = False
, formatConfigUsedSeed = 0
, formatConfigExpectedTotalCount = 0
}

green :: String -> String
green text = setSGRCode [SetColor Foreground Dull Green] <> text <> setSGRCode [Reset]

spec :: Spec
spec = do
  forM_ [
      ("extraChunk", extraChunk, Red)
    , ("missingChunk", missingChunk, Green)
    ] $ \ (name, chunk, color) -> do

    let colorize layer text = setSGRCode [SetColor layer Dull color] <> text <> setSGRCode [Reset]

    describe name $ do
      it "colorizes chunks" $ do
        capture_ $ runFormatM formatConfig $ do
          chunk "foo"
        `shouldReturn` colorize Foreground "foo"

      context "with an all-spaces chunk" $ do
        it "colorizes background" $ do
          capture_ $ runFormatM formatConfig $ do
            chunk "  "
          `shouldReturn` colorize Background "  "

      context "with an all-newlines chunk" $ do
        it "colorizes background" $ do
          capture_ $ runFormatM formatConfig $ do
            chunk "\n\n\n"
          `shouldReturn` colorize Background "\n\n\n"

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
