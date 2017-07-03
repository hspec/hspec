module Test.Hspec.Core.FailureReportSpec (spec) where

import           Helper

import           System.IO
import qualified Control.Exception as E
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.Config

spec :: Spec
spec = do
  describe "writeFailureReport" $ do
    it "prints a warning on unexpected exceptions" $ do
      r <- hCapture_ [stderr] $ writeFailureReport defaultConfig (E.throw (E.ErrorCall "some error"))
      r `shouldBe` "WARNING: Could not write environment variable HSPEC_FAILURES (some error)\n"

  describe "readFailureReport" $ do
    context "when configFailureReport is specified" $ do
      let
        file = "report"
        config = defaultConfig {configFailureReport = Just file}
        report = FailureReport {
            failureReportSeed = 23
          , failureReportMaxSuccess = 42
          , failureReportMaxSize = 65
          , failureReportMaxDiscardRatio = 123
          , failureReportPaths = [(["foo", "bar"], "baz")]
          }
      it "reads a failure report from a file" $ do
        inTempDirectory $ do
          writeFailureReport config report
          readFailureReport config `shouldReturn` Just report

      context "when file does not exist" $ do
        it "returns Nothing" $ do
          inTempDirectory $ do
            readFailureReport config `shouldReturn` Nothing

      context "when file is malformed" $ do
        it "returns Nothing" $ do
          hSilence [stderr] $ inTempDirectory $ do
            writeFile file "foo"
            readFailureReport config `shouldReturn` Nothing

        it "prints a warning" $ do
          inTempDirectory $ do
            writeFile file "foo"
            hCapture_ [stderr] (readFailureReport config) `shouldReturn` "WARNING: Could not read failure report from file \"report\"!\n"
