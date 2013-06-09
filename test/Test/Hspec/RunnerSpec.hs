module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.Meta
import           System.IO.Silently
import           System.IO (stderr)
import           Control.Applicative
import           Control.Monad
import           System.Environment (withArgs, withProgName, getArgs)
import           System.Exit
import           System.Timeout (timeout)
import qualified Control.Exception as E
import           Control.Concurrent (threadDelay)
import           Helper
import           Mock
import           System.SetEnv
import           Test.Hspec.Util (getEnv)
import           Test.QuickCheck

import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as H (Result(..))
import qualified Test.Hspec.Formatters as H (silent)

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` \e -> let _ = e :: ExitCode in return ()

ignoreUserInterrupt :: IO () -> IO ()
ignoreUserInterrupt action = action `E.catch` \e -> unless (e == E.UserInterrupt) (E.throwIO e)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hspec" $ do
    it "runs a spec" $ do
      silence . H.hspec $ do
        H.it "foobar" True
      `shouldReturn` ()

    it "exits with exitFailure if not all examples pass" $ do
      silence . H.hspec $ do
        H.it "foobar" False
      `shouldThrow` (== ExitFailure 1)

    it "allows output to stdout" $ do
      r <- captureLines . H.hspec $ do
        H.it "foobar" $ do
          putStrLn "baz"
      r `shouldSatisfy` elem "baz"

    it "prints an error message on unrecognized command-line options" $ do
      withProgName "myspec" . withArgs ["--foo"] $ do
        hSilence [stderr] (H.hspec $ pure ()) `shouldThrow` (== ExitFailure 1)
        fst `fmap` hCapture [stderr] (ignoreExitCode (H.hspec $ pure ())) `shouldReturn` unlines [
            "myspec: unrecognized option `--foo'"
          , "Try `myspec --help' for more information."
          ]

    it "stores a failure report in the environment" $ do
      silence . ignoreExitCode . withArgs ["--seed", "23"] . H.hspec $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "example 1" True
            H.it "example 2" False
        H.describe "baz" $ do
          H.it "example 3" False
      getEnv "HSPEC_FAILURES" `shouldReturn` Just ("FailureReport {"
        ++ "failureReportSeed = 23, "
        ++ "failureReportMaxSuccess = 100, "
        ++ "failureReportPaths = [([\"foo\",\"bar\"],\"example 2\"),([\"baz\"],\"example 3\")]}")

    describe "with --rerun" $ do
      let runSpec = (captureLines . ignoreExitCode . H.hspec) $ do
            H.it "example 1" True
            H.it "example 2" False
            H.it "example 3" False
            H.it "example 4" True
            H.it "example 5" False

      it "reruns examples that previously failed" $ do
        r0 <- runSpec
        r0 `shouldSatisfy` elem "5 examples, 3 failures"

        r1 <- withArgs ["--rerun"] runSpec
        r1 `shouldSatisfy` elem "3 examples, 3 failures"

      it "reuses the same seed" $ do
        let runSpec_ = (captureLines . ignoreExitCode . H.hspec) $ do
              H.it "foo" $ property $ (/= (26 :: Integer))

        r0 <- withArgs ["--seed", "2413421499272008081"] runSpec_
        r0 `shouldContain` [
            "Falsifiable (after 66 tests): "
          , "26"
          ]

        r1 <- withArgs ["-r"] runSpec_
        r1 `shouldContain` [
            "Falsifiable (after 66 tests): "
          , "26"
          ]

      it "reuses same --qc-max-success" $ do
        silence . ignoreExitCode . withArgs ["--qc-max-success", "23"] . H.hspec $ do
          H.it "foo" False

        m <- newMock
        silence . withArgs ["--rerun"] . H.hspec $ do
          H.it "foo" $ property $ do
            mockAction m
        mockCounter m `shouldReturn` 23

      context "when there is no failure report in the environment" $ do
        it "runs everything" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hSilence [stderr] $ withArgs ["-r"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hCapture_ [stderr] $ withArgs ["-r"] runSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"

      context "when parsing of failure report fails" $ do
        it "runs everything" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hSilence [stderr] $ withArgs ["-r"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hCapture [stderr] $ withArgs ["-r"] runSpec
          fst r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"


    it "does not leak command-line flags to examples" $ do
      silence . withArgs ["--verbose"] $ do
        H.hspec $ do
          H.it "foobar" $ do
            getArgs `shouldReturn` []
        `shouldReturn` ()

    context "when interrupted with ctrl-c" $ do
      it "prints summary immediately" $ do
        r <- captureLines . ignoreUserInterrupt . withArgs ["--seed", "23"] . H.hspec $ do
          H.it "foo" False
          H.it "bar" $ do
            E.throwIO E.UserInterrupt :: IO ()
          H.it "baz" True
        normalizeSummary r `shouldBe` [
            ""
          , "- foo FAILED [1]"
          , ""
          , "1) foo FAILED"
          , ""
          , "Randomized with seed 23"
          , ""
          ]

      it "throws UserInterrupt" $ do
        silence . H.hspec $ do
          H.it "foo" $ do
            E.throwIO E.UserInterrupt :: IO ()
        `shouldThrow` (== E.UserInterrupt)

    context "with --help" $ do
      let printHelp = withProgName "spec" . withArgs ["--help"] . H.hspec $ pure ()
      it "prints help" $ do
        r <- (captureLines . ignoreExitCode) printHelp
        r `shouldStartWith` ["Usage: spec [OPTION]..."]
        silence printHelp `shouldThrow` (== ExitSuccess)

      it "constrains lines to 80 characters" $ do
        r <- (captureLines . ignoreExitCode) printHelp
        r `shouldSatisfy` all ((<= 80) . length)
        r `shouldSatisfy` any ((78 <=) . length)

    context "with --dry-run" $ do
      it "produces a report" $ do
        r <- captureLines . withArgs ["--dry-run"] . H.hspec $ do
          H.it "foo" True
          H.it "bar" True
        normalizeSummary r `shouldBe` [
            ""
          , "- foo"
          , "- bar"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 0 failures"
          ]

      it "does not verify anything" $ do
        e <- newMock
        _ <- captureLines . withArgs ["--dry-run"] . H.hspec $ do
          H.it "foo" (mockAction e)
          H.it "bar" False
        mockCounter e `shouldReturn` 0

    context "with --fail-fast" $ do
      it "stops after first failure" $ do
        r <- captureLines . ignoreExitCode . withArgs ["--fail-fast", "--seed", "23"] . H.hspec $ do
          H.it "foo" True
          H.it "bar" False
          H.it "baz" False
        normalizeSummary r `shouldBe` [
            ""
          , "- foo"
          , "- bar FAILED [1]"
          , ""
          , "1) bar FAILED"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 1 failure"
          ]

      it "works for nested specs" $ do
        r <- captureLines . ignoreExitCode . withArgs ["--fail-fast", "--seed", "23"] . H.hspec $ do
          H.describe "foo" $ do
            H.it "bar" False
            H.it "baz" True
        normalizeSummary r `shouldBe` [
            ""
          , "foo"
          , "  - bar FAILED [1]"
          , ""
          , "1) foo bar FAILED"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --match" $ do
      it "only runs examples that match a given pattern" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        silence . withArgs ["-m", "/bar/example"] . H.hspec $ do
          H.describe "foo" $ do
            H.describe "bar" $ do
              H.it "example 1" $ mockAction e1
              H.it "example 2" $ mockAction e2
            H.describe "baz" $ do
              H.it "example 3" $ mockAction e3
        (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 1, 0)

      it "can be given multiple times" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        silence . withArgs ["-m", "foo", "-m", "baz"] . H.hspec $ do
          H.describe "foo" $ do
            H.it "example 1" $ mockAction e1
          H.describe "bar" $ do
            H.it "example 2" $ mockAction e2
          H.describe "baz" $ do
            H.it "example 3" $ mockAction e3
        (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 0, 1)

    context "with --format" $ do
      it "uses specified formatter" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--format", "progress"] . H.hspec $ do
          H.it "foo" True
          H.it "bar" True
          H.it "baz" False
          H.it "qux" True
        r `shouldContain` "..F."

      context "when given an invalid argument" $ do
        it "prints an error message to stderr" $ do
          r <- hCapture_ [stderr] . ignoreExitCode . withArgs ["--format", "foo"] . H.hspec $ do
            H.it "foo" True
          r `shouldContain` "invalid argument `foo' for `--format'"

    context "with --qc-max-success" $ do
      it "tries QuickCheck properties specified number of times" $ do
        m <- newMock
        silence . withArgs ["--qc-max-success", "23"] . H.hspec $ do
          H.it "foo" $ property $ do
            mockAction m
        mockCounter m `shouldReturn` 23

      context "when run with --rerun" $ do
        it "takes precedence" $ do
          silence . ignoreExitCode . withArgs ["--qc-max-success", "23"] . H.hspec $ do
            H.it "foo" False

          m <- newMock
          silence . withArgs ["--rerun", "--qc-max-success", "42"] . H.hspec $ do
            H.it "foo" $ property $ do
              mockAction m
          mockCounter m `shouldReturn` 42

      context "when given an invalid argument" $ do
        it "prints an error message to stderr" $ do
          r <- hCapture_ [stderr] . ignoreExitCode . withArgs ["--qc-max-success", "foo"] . H.hspec $ do
            H.it "foo" True
          r `shouldContain` "invalid argument `foo' for `--qc-max-success'"

    context "with --seed" $ do
      it "uses specified seed" $ do
        r <- captureLines . ignoreExitCode . withArgs ["--seed", "2413421499272008081"] . H.hspec $ do
            H.it "foo" $
              property (/= (26 :: Integer))
        r `shouldContain` [
            "Falsifiable (after 66 tests): "
          , "26"
          ]

      context "when run with --rerun" $ do
        it "takes precedence" $ do
          let runSpec args = capture_ . ignoreExitCode . withArgs args . H.hspec $ do
                H.it "foo" $
                  property $ \n -> ((17 + 31 * n) `mod` 50) /= (23 :: Integer)
          r0 <- runSpec ["--seed", "23"]
          r0 `shouldContain` "(after 88 tests)"

          r1 <- runSpec ["--seed", "42"]
          r1 `shouldContain` "(after 48 tests)"

          r2 <- runSpec ["--rerun", "--seed", "23"]
          r2 `shouldContain` "(after 88 tests)"

      context "when given an invalid argument" $ do
        let run = withArgs ["--seed", "foo"] . H.hspec $ do
                    H.it "foo" True
        it "prints an error message to stderr" $ do
          r <- hCapture_ [stderr] (ignoreExitCode run)
          r `shouldContain` "invalid argument `foo' for `--seed'"

        it "exits with exitFailure" $ do
          hSilence [stderr] run `shouldThrow` (== ExitFailure 1)

    context "with --print-cpu-time" $ do
      it "includes used CPU time in summary" $ do
        r <- capture_ $ withArgs ["--print-cpu-time"] (H.hspec $ pure ())
        (normalizeSummary . lines) r `shouldContain` ["Finished in 0.0000 seconds, used 0.0000 seconds of CPU time"]

    context "with --html" $ do
      it "produces HTML output" $ do
        r <- capture_ . withArgs ["--html"] . H.hspec $ do
          H.it "foo" True
        r `shouldContain` "</span>"

      it "marks successful examples with CSS class hspec-success" $ do
        r <- capture_ . withArgs ["--html"] . H.hspec $ do
          H.it "foo" True
        r `shouldContain` "<span class=\"hspec-success\">- foo\n</span>"

      it "marks pending examples with CSS class hspec-pending" $ do
        r <- capture_ . withArgs ["--html"] . H.hspec $ do
          H.it "foo" H.pending
        r `shouldContain` "<span class=\"hspec-pending\">- foo"

      it "marks failed examples with CSS class hspec-failure" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--html"] . H.hspec $ do
          H.it "foo" False
        r `shouldContain` "<span class=\"hspec-failure\">- foo"

  describe "hspecResult" $ do
    it "returns a summary of the test run" $ do
      silence . H.hspecResult $ do
        H.it "foo" True
        H.it "foo" False
        H.it "foo" False
        H.it "foo" True
        H.it "foo" True
      `shouldReturn` H.Summary 5 2

    it "treats uncaught exceptions as failure" $ do
      silence . H.hspecResult  $ do
        H.it "foobar" (E.throwIO (E.ErrorCall "foobar") >> pure ())
      `shouldReturn` H.Summary 1 1

    it "uses the specdoc formatter by default" $ do
      _:r:_ <- captureLines . H.hspecResult $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      r <- capture_ . H.hspecWith H.defaultConfig {H.configFormatter = H.silent} $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` ""

    it "does not let escape error thunks from failure messages" $ do
      r <- silence . H.hspecResult $ do
        H.it "some example" (H.Fail $ "foobar" ++ undefined)
      r `shouldBe` H.Summary 1 1

    it "runs specs in parallel" $ do
      let n = 10
          t = 10000
          dt = t * (n `div` 2)
      r <- timeout dt . silence . H.hspecResult . H.parallel $ do
        replicateM_ n (H.it "foo" $ threadDelay t)
      r `shouldBe` Just (H.Summary n 0)
