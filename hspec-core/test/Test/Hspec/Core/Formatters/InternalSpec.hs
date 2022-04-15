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
, formatConfigUseDiff = False
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
  describe "write" $ do
    it "does not span colored output over multiple lines" $ do
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
