module Test.Hspec.FailureReportSpec (main, spec) where

import           Test.Hspec.Meta

import           System.IO
import           System.IO.Silently
import           Test.Hspec.FailureReport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeFailureReport" $ do
    it "prints a warning on unexpected exceptions" $ do
      (r, ()) <- hCapture [stderr] $ writeFailureReport (error "some error")
      r `shouldBe` "WARNING: Could not write environment variable HSPEC_FAILURES (some error)\n"
