module Test.Hspec.FailureReportSpec (main, spec) where

import           Test.Hspec.Meta

import           System.IO
import           System.IO.Silently
import           Test.Hspec.FailureReport
import           GHC.Paths (ghc)
import           System.Process
import           System.Exit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeFailureReport" $ do
    it "prints a warning on unexpected exceptions" $ do
      r <- hCapture_ [stderr] $ writeFailureReport (error "some error")
      r `shouldBe` "WARNING: Could not write environment variable HSPEC_FAILURES (some error)\n"

  -- GHCi needs to keep the environment on :reload, so that we can store
  -- failures there.  Otherwise --re-run would not be very useful.  So we add a
  -- test for that.
  describe "GHCi" $ do
    it "keeps environment variables on :reload" $ do
      let flags = ["-v0", "--interactive", "-ignore-dot-ghci"]
      (Just hIn, Just hOut, Nothing, processHandle) <- createProcess $ (proc ghc flags) {
          std_in = CreatePipe
        , std_out = CreatePipe
        }
      hPutStrLn hIn "import System.SetEnv"
      hPutStrLn hIn "setEnv \"FOO\" \"bar\""
      hPutStrLn hIn ":reload"
      hPutStrLn hIn "import System.Environment"
      hPutStrLn hIn "getEnv \"FOO\""
      hClose hIn
      r <- hGetContents hOut
      length r `seq` r `shouldBe` "\"bar\"\n"
      waitForProcess processHandle `shouldReturn` ExitSuccess
