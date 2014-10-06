module Test.Hspec.RunnerSpec (main, spec) where

import           Helper
import           System.IO (stderr)
import           Control.Monad
import           System.Environment (withArgs, withProgName, getArgs)
import           System.Exit
import           Control.Concurrent
import qualified Control.Exception as E
import           Mock
import           System.SetEnv
import           Test.Hspec.Compat

import           Test.Hspec.FailureReport (FailureReport(..))
import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as H (Result(..))
import qualified Test.Hspec.Formatters as H (silent)
import qualified Test.Hspec.QuickCheck as H

import qualified Test.QuickCheck as QC

main :: IO ()
main = hspec spec

quickCheckOptions :: [([Char], Args -> Int)]
quickCheckOptions = [("--qc-max-success", QC.maxSuccess), ("--qc-max-size", QC.maxSize), ("--qc-max-discard", QC.maxDiscardRatio)]

runPropFoo :: [String] -> IO String
runPropFoo args = unlines . normalizeSummary . lines <$> do
  capture_ . ignoreExitCode . withArgs args . H.hspec .  H.modifyMaxSuccess (const 1000000) $ do
    H.it "foo" $ do
      property (/= (23 :: Int))

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
      lookupEnv "HSPEC_FAILURES" `shouldReturn` (Just . show) FailureReport {
          failureReportSeed = 23
        , failureReportMaxSuccess = 100
        , failureReportMaxSize = 100
        , failureReportMaxDiscardRatio = 10
        , failureReportPaths = [
            (["foo", "bar"], "example 2")
          , (["baz"], "example 3")
          ]
        }

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
        r <- runPropFoo ["--seed", "42"]
        runPropFoo ["-r"] `shouldReturn` r

      forM_ quickCheckOptions $ \(flag, accessor) -> do
        it ("reuses same " ++ flag) $ do
          [flag, "23"] `shouldUseArgs` ((== 23) . accessor)
          ["--rerun"] `shouldUseArgs` ((== 23) . accessor)

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
          r <- hCapture_ [stderr] $ withArgs ["-r"] runSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"


    it "does not leak command-line flags to examples" $ do
      silence . withArgs ["--verbose"] $ do
        H.hspec $ do
          H.it "foobar" $ do
            getArgs `shouldReturn` []
        `shouldReturn` ()

    context "when interrupted with ctrl-c" $ do
      it "prints summary immediately" $ do
        mvar <- newEmptyMVar
        sync <- newEmptyMVar
        threadId <- forkIO $ do
          r <- captureLines . ignoreUserInterrupt . withArgs ["--seed", "23"] . H.hspec $ do
            H.it "foo" False
            H.it "bar" $ do
              putMVar sync ()
              threadDelay 1000000
            H.it "baz" True
          putMVar mvar r
        takeMVar sync
        throwTo threadId E.UserInterrupt
        r <- takeMVar mvar
        normalizeSummary r `shouldBe` [
            ""
          , "- foo FAILED [1]"
          , ""
          , "1) foo"
          , ""
          , "Randomized with seed 23"
          , ""
          ]

      it "throws UserInterrupt" $ do
        mvar <- newEmptyMVar
        sync <- newEmptyMVar
        threadId <- forkIO $ do
          silence . H.hspec $ do
            H.it "foo" $ do
              putMVar sync ()
              threadDelay 1000000
          `E.catch` putMVar mvar
        takeMVar sync
        throwTo threadId E.UserInterrupt
        takeMVar mvar `shouldReturn` E.UserInterrupt

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
      let withDryRun = captureLines . withArgs ["--dry-run"] . H.hspec
      it "produces a report" $ do
        r <- withDryRun $ do
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
        _ <- withDryRun $ do
          H.it "foo" (mockAction e)
          H.it "bar" False
        mockCounter e `shouldReturn` 0

      it "ignores afterAll-hooks" $ do
        ref <- newIORef False
        _ <- withDryRun $ do
          H.afterAll_ (writeIORef ref True) $ do
            H.it "bar" True
        readIORef ref `shouldReturn` False

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
          , "1) bar"
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
          , "1) foo bar"
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
          ["--qc-max-success", "23"] `shouldUseArgs` ((== 23) . QC.maxSuccess)
          ["--rerun", "--qc-max-success", "42"] `shouldUseArgs` ((== 42) . QC.maxSuccess)

    context "with --qc-max-size" $ do
      it "passes specified size to QuickCheck properties" $ do
        ["--qc-max-size", "23"] `shouldUseArgs` ((== 23) . QC.maxSize)

    context "with --qc-max-discard" $ do
      it "uses specified discard ratio to QuickCheck properties" $ do
        ["--qc-max-discard", "23"] `shouldUseArgs` ((== 23) . QC.maxDiscardRatio)

    context "with --seed" $ do
      it "uses specified seed" $ do
        r <- runPropFoo ["--seed", "42"]
        runPropFoo ["--seed", "42"] `shouldReturn` r

      context "when run with --rerun" $ do
        it "takes precedence" $ do
          r <- runPropFoo ["--seed", "23"]
          _ <- runPropFoo ["--seed", "42"]
          runPropFoo ["--rerun", "--seed", "23"] `shouldReturn` r

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
        H.it "foobar" throwException
      `shouldReturn` H.Summary 1 1

    it "uses the specdoc formatter by default" $ do
      _:r:_ <- captureLines . H.hspecResult $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      r <- capture_ . H.hspecWithResult H.defaultConfig {H.configFormatter = Just H.silent} $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` ""

    it "does not let escape error thunks from failure messages" $ do
      r <- silence . H.hspecResult $ do
        H.it "some example" (H.Fail $ "foobar" ++ undefined)
      r `shouldBe` H.Summary 1 1

    it "runs specs in parallel" $ do
      let n = 10
          t = 0.01
          dt = t * (fromIntegral n / 2)
      r <- timeout dt . silence . H.hspecResult . H.parallel $ do
        replicateM_ n (H.it "foo" $ sleep t)
      r `shouldBe` Just (H.Summary n 0)
