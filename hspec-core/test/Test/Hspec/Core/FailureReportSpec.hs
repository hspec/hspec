module Test.Hspec.Core.FailureReportSpec (main, spec) where

import           Helper

import           System.IO
import qualified Control.Exception as E
import           Test.Hspec.Core.FailureReport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeFailureReport" $ do
    it "prints a warning on unexpected exceptions" $ do
      r <- hCapture_ [stderr] $ writeFailureReport (E.throw (E.ErrorCall "some error"))
      r `shouldBe` "WARNING: Could not write environment variable HSPEC_FAILURES (some error)\n"
