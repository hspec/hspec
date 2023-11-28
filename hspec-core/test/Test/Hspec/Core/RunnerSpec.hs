{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.Core.RunnerSpec (spec) where

import           Prelude ()
import           Helper

import           System.IO
import           System.Environment (withProgName, getArgs, setEnv, unsetEnv)
import           System.Exit
import           Control.Concurrent
import           Control.Concurrent.Async
import           Mock

import           Test.Hspec.Core.FailureReport (FailureReport(..))
import qualified Test.Hspec.Expectations as H
import qualified Test.Hspec.Core.Spec as H
import           Test.Hspec.Core.Runner (UseColor(..), ProgressReporting(..))
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Runner.Result
import qualified Test.Hspec.Core.QuickCheck as H

import qualified Test.QuickCheck as QC
import qualified Test.Hspec.Core.Hooks as H

import           Test.Hspec.Core.Formatters.Pretty.ParserSpec (Person(..))

runPropFoo :: [String] -> IO String
runPropFoo args = unlines . normalizeSummary . lines <$> do
  capture_ . ignoreExitCode . withArgs args . H.hspec .  H.modifyMaxSuccess (const 1000000) $ do
    H.it "foo" $ do
      property (/= (23 :: Int))

person :: Int -> Person
person = Person "Joe"

data MyException = MyException
  deriving (Eq, Show)

instance Exception MyException where
  displayException MyException = "my exception"

resultWithColorizedReason :: H.Result
resultWithColorizedReason = H.Result {
  H.resultInfo = "info"
, H.resultStatus = H.Failure Nothing . H.ColorizedReason $ "some " <> green "colorized" <> " error message"
}

spec :: Spec
spec = do
  describe "hspec" $ do

    let
      hspec args = withArgs args . hspecSilent
      hspec_ = hspecSilent

    it "evaluates examples Unmasked" $ do
      mvar <- newEmptyMVar
      hspec_ $ do
        H.it "foo" $ do
          getMaskingState >>= putMVar mvar
      takeMVar mvar `shouldReturn` Unmasked

    it "runs finalizers" $ do
      mvar <- newEmptyMVar
      ref <- newIORef "did not run finalizer"
      a <- async $ hspec_ $ do
          H.it "foo" $ do
            (putMVar mvar () >> threadDelay 10000000) `finally`
              writeIORef ref "ran finalizer"
      takeMVar mvar
      cancel a
      readIORef ref `shouldReturn` "ran finalizer"

    it "runs a spec" $ do
      hspec_ $ do
        H.it "foobar" True
      `shouldReturn` ()

    it "exits with exitFailure if not all examples pass" $ do
      hspec_ $ do
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
      ignoreExitCode . hspec ["--seed", "23"] $ do
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

    context "with --rerun" $ do
      let
        failingSpec = do
          H.it "example 1" True
          H.it "example 2" False
          H.it "example 3" False
          H.it "example 4" True
          H.it "example 5" False

        succeedingSpec = do
          H.it "example 1" True
          H.it "example 2" True
          H.it "example 3" True
          H.it "example 4" True
          H.it "example 5" True

        run = captureLines . H.hspecResult
        rerun = withArgs ["--rerun"] . run

      it "reuses same --seed" $ do
        r <- runPropFoo ["--seed", "42"]
        runPropFoo ["--rerun"] `shouldReturn` r

      it "reuses same --qc-max-success" $ do
        n <- generate arbitrary
        ["--qc-max-success", show n] `shouldUseArgs` (QC.maxSuccess, n)
        ["--rerun"] `shouldUseArgs` (QC.maxSuccess, n)

      it "reuses same --qc-max-discard" $ do
        n <- generate arbitrary
        ["--qc-max-discard", show n] `shouldUseArgs` (QC.maxDiscardRatio, n)
        ["--rerun"] `shouldUseArgs` (QC.maxDiscardRatio, n)

      it "reuses same --qc-max-size" $ do
        n <- generate arbitrary
        ["--qc-max-size", show n] `shouldUseArgs` (QC.maxSize, n)
        ["--rerun"] `shouldUseArgs` (QC.maxSize, n)

      context "with failing examples" $ do
        it "only reruns failing examples" $ do
          r0 <- run failingSpec
          last r0 `shouldBe` "5 examples, 3 failures"

          r1 <- rerun failingSpec
          last r1 `shouldBe` "3 examples, 3 failures"

      context "without failing examples" $ do
        it "runs all examples" $ do
          r0 <- run succeedingSpec
          last r0 `shouldBe` "5 examples, 0 failures"

          r1 <- rerun succeedingSpec
          last r1 `shouldBe` "5 examples, 0 failures"

      context "when there is no failure report in the environment" $ do
        it "runs all examples" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hSilence [stderr] $ rerun failingSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hCapture_ [stderr] $ rerun failingSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"

      context "when parsing of failure report fails" $ do
        it "runs all examples" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hSilence [stderr] $ rerun failingSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hCapture_ [stderr] $ rerun failingSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"

      context "with --rerun-all-on-success" $ do
        let rerunAllOnSuccess = withArgs ["--rerun", "--rerun-all-on-success"] . run
        context "after a previously failing rerun succeeds for the first time" $ do
          it "runs the whole test suite" $ do
            _ <- run failingSpec
            output <- rerunAllOnSuccess succeedingSpec
            output `shouldSatisfy` elem "3 examples, 0 failures"
            last output `shouldBe` "5 examples, 0 failures"

          it "reruns runIO-actions" $ do
            ref <- newIORef (0 :: Int)
            let succeedingSpecWithRunIO = H.runIO (modifyIORef ref succ) >> succeedingSpec

            _ <- run failingSpec
            _ <- rerunAllOnSuccess succeedingSpecWithRunIO
            readIORef ref `shouldReturn` 2

    it "does not leak command-line options to examples" $ do
      hspec ["--diff"] $ do
        H.it "foobar" $ do
          getArgs `shouldReturn` []
      `shouldReturn` ()

    context "when interrupted with ctrl-c" $ do
      it "prints summary immediately" $ do
        mvar <- newEmptyMVar
        sync <- newEmptyMVar
        threadId <- forkIO $ do
          r <- captureLines . ignoreUserInterrupt . withArgs ["--seed", "23"] . H.hspec . removeLocations $ do
            H.it "foo" False
            H.it "bar" $ do
              putMVar sync ()
              threadDelay 1000000
            H.it "baz" True
          putMVar mvar r
        takeMVar sync
        throwTo threadId UserInterrupt
        r <- takeMVar mvar
        normalizeSummary r `shouldBe` [
            ""
          , "foo [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) foo"
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

      it "throws UserInterrupt" $ do
        mvar <- newEmptyMVar
        sync <- newEmptyMVar
        threadId <- forkIO $ do
          hspec_ $ do
            H.it "foo" $ do
              putMVar sync ()
              threadDelay 1000000
          `catch` putMVar mvar
        takeMVar sync
        throwTo threadId UserInterrupt
        takeMVar mvar `shouldReturn` UserInterrupt

    context "with --dry-run" $ do
      let withDryRun = hspecCapture ["--dry-run"]
      it "produces a report" $ do
        withDryRun $ do
          H.it "foo" True
          H.it "bar" True
        `shouldReturn` unlines [
            ""
          , "foo [✔]"
          , "bar [✔]"
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

    context "with --focused-only" $ do
      let run = hspecCapture ["--focused-only"]
      context "when there aren't any focused spec items" $ do
        it "does not run anything" $ do
          run $ do
            H.it "foo" True
            H.it "bar" True
          `shouldReturn` unlines [
              ""
            , ""
            , "Finished in 0.0000 seconds"
            , "0 examples, 0 failures"
            ]

    context "with --fail-on=empty" $ do
      it "fails if no spec items have been run" $ do
        (out, r) <- hCapture [stdout, stderr] . try . withProgName "spec" . withArgs ["--skip=", "--fail-on=empty"] . H.hspec $ do
          H.it "foo" True
          H.it "bar" True
          H.it "baz" True
        unlines (normalizeSummary (lines out)) `shouldBe` unlines [
            "spec: all spec items have been filtered; failing due to --fail-on=empty"
          ]
        r `shouldBe` Left (ExitFailure 1)

    context "with --fail-on=focused" $ do
      let run = hspecCapture ["--fail-on=focused", "--seed", "23"]
      it "fails on focused spec items" $ do
        run $ do
          H.it "foo" True
          H.fit "bar" True
        `shouldReturn` unlines [
            ""
          , "bar [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) bar"
          , "       item is focused; failing due to --fail-on=focused"
          , ""
          , "  To rerun use: --match \"/bar/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --fail-on=empty-description" $ do
      let run = hspecCapture ["--fail-on=empty-description", "--seed", "23"]
      it "fails on items with empty requirement" $ do
        run $ do
          H.it "foo" True
          H.it "" True
        `shouldReturn` unlines [
            ""
          , "foo [✔]"
          , "(unspecified behavior) [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) (unspecified behavior)"
          , "       item has no description; failing due to --fail-on=empty-description"
          , ""
          , "  To rerun use: --match \"/(unspecified behavior)/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 1 failure"
          ]

    context "with --fail-on=pending" $ do
      let run = hspecCapture ["--fail-on=pending", "--seed", "23"]
      it "fails on pending spec items" $ do
        run $ do
          H.it "foo" True
          H.it "bar" $ do
            void $ throwIO (H.Pending Nothing Nothing)
        `shouldReturn` unlines [
            ""
          , "foo [✔]"
          , "bar [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) bar"
          , "       item is pending; failing due to --fail-on=pending"
          , ""
          , "  To rerun use: --match \"/bar/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 1 failure"
          ]

    context "with --fail-fast" $ do
      it "stops after first failure" $ do
        hspecCapture ["--fail-fast", "--seed", "23"] $ do
          H.it "foo" True
          H.it "bar" False
          H.it "baz" False
        `shouldReturn` unlines [
            ""
          , "foo [✔]"
          , "bar [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) bar"
          , ""
          , "  To rerun use: --match \"/bar/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 1 failure"
          ]

      it "cancels running parallel spec items on failure" $ do
        child1 <- newQSem 0
        child2 <- newQSem 0
        parent <- newQSem 0
        ref <- newIORef ""
        ignoreExitCode . hspec ["--fail-fast", "-j", "2"] $ do
          H.parallel $ do
            H.it "foo" $ do
              waitQSem child1
              "foo" `shouldBe` "bar"
            H.it "bar" $ do
              -- NOTE: waitQSem should never return here, as we want to
              -- guarantee that the thread is killed before hspec returns
              (signalQSem child1 >> waitQSem child2 >> writeIORef ref "foo") `finally` signalQSem parent
        signalQSem child2
        waitQSem parent
        readIORef ref `shouldReturn` ""

      it "works for nested specs" $ do
        hspecCapture ["--fail-fast", "--seed", "23"] $ do
          H.describe "foo" $ do
            H.it "bar" False
            H.it "baz" True
        `shouldReturn` unlines [
            ""
          , "foo"
          , "  bar [✘]"
          , ""
          , "Failures:"
          , ""
          , "  1) foo bar"
          , ""
          , "  To rerun use: --match \"/foo/bar/\" --seed 23"
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

      context "with a cleanup action" $ do
        it "runs cleanup action" $ do
          ref <- newIORef 0
          ignoreExitCode $ hspec ["--fail-fast"] $ do
            H.afterAll_ (modifyIORef ref succ) $ do
              H.it "foo" True
              H.it "bar" False
              H.it "baz" True
          readIORef ref `shouldReturn` (1 :: Int)

        context "when last leaf fails" $ do
          it "runs cleanup action exactly once" $ do
            ref <- newIORef 0
            ignoreExitCode $ hspec ["--fail-fast"] $ do
              H.afterAll_ (modifyIORef ref succ) $ do
                H.it "foo" True
                H.it "bar" True
                H.it "baz" False
            readIORef ref `shouldReturn` (1 :: Int)

    context "with --match" $ do
      it "only runs examples that match a given pattern" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        hspec ["-m", "/bar/example"] $ do
          H.describe "foo" $ do
            H.describe "bar" $ do
              H.it "example 1" $ mockAction e1
              H.it "example 2" $ mockAction e2
            H.describe "baz" $ do
              H.it "example 3" $ mockAction e3
        (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 1, 0)

      it "only runs examples that match a given pattern (-m and --skip combined)" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        e4 <- newMock
        hspec ["-m", "/bar/example", "--skip", "example 3"] $ do
          H.describe "foo" $ do
            H.describe "bar" $ do
              H.it "example 1" $ mockAction e1
              H.it "example 2" $ mockAction e2
              H.it "example 3" $ mockAction e3
            H.describe "baz" $ do
              H.it "example 4" $ mockAction e4
        (,,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 <*> mockCounter e4
          `shouldReturn` (1, 1, 0, 0)

      it "can be given multiple times" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        hspec ["-m", "foo", "-m", "baz"] $ do
          H.describe "foo" $ do
            H.it "example 1" $ mockAction e1
          H.describe "bar" $ do
            H.it "example 2" $ mockAction e2
          H.describe "baz" $ do
            H.it "example 3" $ mockAction e3
        (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 0, 1)

#if __GLASGOW_HASKELL__ >= 802
    context "with --pretty" $ do
      it "pretty-prints Haskell values" $ do
        let args = ["--pretty", "--seed=0", "--format=failed-examples"]
        r <- fmap (unlines . normalizeSummary . lines) . capture_ . ignoreExitCode . withArgs args . H.hspec . removeLocations $ do
          H.it "foo" $ do
            person 23 `H.shouldBe` person 42
        r `shouldBe` unlines [
            ""
          , "Failures:"
          , ""
          , "  1) foo"
          , "       expected: Person {"
          , "                   personName = \"Joe\","
          , "                   personAge = 42"
          , "                 }"
          , "        but got: Person {"
          , "                   personName = \"Joe\","
          , "                   personAge = 23"
          , "                 }"
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]
#endif

      it "uses custom pretty-print functions" $ do
        let pretty _ _ _ = ("foo", "bar")

        r <- capture_ . ignoreExitCode . withArgs ["--pretty"] . H.hspec $ do
          H.modifyConfig $ \ c -> c { H.configPrettyPrintFunction = pretty }
          H.it "foo" $ do
            23 `H.shouldBe` (42 :: Int)
        r `shouldContain` unlines [
            "       expected: foo"
          , "        but got: bar"
          ]

    context "with --no-pretty" $ do
      it "does not pretty-prints Haskell values" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--no-pretty"] . H.hspec $ do
          H.it "foo" $ do
            person 23 `H.shouldBe` person 42
        r `shouldContain` unlines [
            "       expected: Person {personName = \"Joe\", personAge = 42}"
          , "        but got: Person {personName = \"Joe\", personAge = 23}"
          ]

    context "when formatting exceptions" $ do
      let spec_ = H.it "foo" $ void (throwIO MyException)
      context "with --show-exceptions" $ do
        it "uses `show`" $ do
          hspecCapture ["--seed=0", "--format=failed-examples", "--display-exceptions", "--show-exceptions"] spec_
          `shouldReturn` unlines [
              ""
            , "Failures:"
            , ""
            , "  1) foo"
            , "       uncaught exception: MyException"
            , "       MyException"
            , ""
            , "  To rerun use: --match \"/foo/\" --seed 0"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 1 failure"
            ]

      context "with --display-exceptions" $ do
        it "uses `displayException`" $ do
          hspecCapture ["--seed=0", "--format=failed-examples", "--show-exceptions", "--display-exceptions"] spec_
          `shouldReturn` unlines [
              ""
            , "Failures:"
            , ""
            , "  1) foo"
            , "       uncaught exception: MyException"
            , "       my exception"
            , ""
            , "  To rerun use: --match \"/foo/\" --seed 0"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 1 failure"
            ]

    context "with --color" $ do
      it "outputs ColorizedReason" $ do
        hspecCapture ["--seed=0", "--format=failed-examples", "--color"] $ do
          H.it "foo" resultWithColorizedReason
        `shouldReturn` unlines [
            "\ESC[?25l"
          , "Failures:"
          , ""
          , "  1) foo"
          , "       some " ++ green "colorized" ++ " error message"
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , red "1 example, 1 failure"
          , "\ESC[?25h"
          ]

    context "with --no-color" $ do
      it "strips ANSI sequences from ColorizedReason" $ do
        hspecCapture ["--seed=0", "--format=failed-examples", "--no-color"] $ do
          H.it "foo" resultWithColorizedReason
        `shouldReturn` unlines [
            ""
          , "Failures:"
          , ""
          , "  1) foo"
          , "       some colorized error message"
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --diff" $ do
      it "shows colorized diffs" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--diff", "--color"] . H.hspec $ do
          H.it "foo" $ do
            23 `H.shouldBe` (42 :: Int)
        r `shouldContain` unlines [
            red "       expected: " ++ red "42"
          , red "        but got: " ++ green "23"
          ]

    context "with --no-diff" $ do
      it "it does not show colorized diffs" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--no-diff", "--color"] . H.hspec $ do
          H.it "foo" $ do
            23 `H.shouldBe` (42 :: Int)
        r `shouldContain` unlines [
            red "       expected: " ++ "42"
          , red "        but got: " ++ "23"
          ]

    context "with --diff-context" $ do
      it "suppresses excessive diff output" $ do
        let
          args = ["--seed=0", "--format=failed-examples", "--diff-context=1"]
          expected = map show [1 .. 99 :: Int]
          actual = replace "50" "foo" expected
        r <- fmap (unlines . normalizeSummary . lines) . capture_ . ignoreExitCode . withArgs args . H.hspec . removeLocations $ do
          H.it "foo" $ do
            unlines actual `H.shouldBe` unlines expected
        r `shouldBe` unlines [
            ""
          , "Failures:"
          , ""
          , "  1) foo"
          , "       expected: @@ 48 lines omitted @@"
          , "                 49"
          , "                 50"
          , "                 51"
          , "                 @@ 48 lines omitted @@"
          , "                 "
          , "        but got: @@ 48 lines omitted @@"
          , "                 49"
          , "                 foo"
          , "                 51"
          , "                 @@ 48 lines omitted @@"
          , "                 "
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --diff-command" $ do
      it "uses an external diff command" $ do
        let
          args = ["--seed=0", "--format=failed-examples", "--diff-command", "diff -u -L expected -L actual"]
          expected = map show [1 .. 99 :: Int]
          actual = replace "50" "foo" expected
        r <- fmap (unlines . normalizeSummary . lines) . capture_ . ignoreExitCode . withArgs args . H.hspec . removeLocations $ do
          H.it "foo" $ do
            unlines actual `H.shouldBe` unlines expected
        r `shouldBe` unlines [
            ""
          , "Failures:"
          , ""
          , "  1) foo"
          , "--- expected"
          , "+++ actual"
          , "@@ -47,7 +47,7 @@"
          , " 47"
          , " 48"
          , " 49"
          , "-50"
          , "+foo"
          , " 51"
          , " 52"
          , " 53"
          , ""
          , "  To rerun use: --match \"/foo/\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --print-slow-items" $ do
      it "prints slow items" $ do
        r <- fmap lines . hCapture_ [stdout, stderr] . ignoreExitCode . withArgs ["--print-slow-items"] . H.hspec $ do
          H.it "foo" $ threadDelay 2000
        normalizeTimes (normalizeSummary r) `shouldBe` [
            ""
          , "foo [✔]"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 0 failures"
          , ""
          , "Slow spec items:"
          , "  test" </> "Test" </> "Hspec" </> "Core" </> "RunnerSpec.hs:" <> show (__LINE__ - 9 :: Int) <> ":11: /foo/ (2ms)"
          ]

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
      let
        run :: HasCallStack => String -> IO ()
        run option = do
          m <- newMock
          hspec [option, "23"] $ do
            H.it "foo" $ property $ \(_ :: Int) -> do
              mockAction m
          mockCounter m `shouldReturn` 23

      it "tries QuickCheck properties specified number of times" $ do
        run "--qc-max-success"

      it "accepts --maximum-generated-tests as an alias" $ do
        run "--maximum-generated-tests"

      context "when run with --rerun" $ do
        it "takes precedence" $ do
          ["--qc-max-success", "23"] `shouldUseArgs` (QC.maxSuccess, 23)
          ["--rerun", "--qc-max-success", "42"] `shouldUseArgs` (QC.maxSuccess, 42)

    context "with --qc-max-size" $ do
      it "passes specified size to QuickCheck properties" $ do
        ["--qc-max-size", "23"] `shouldUseArgs` (QC.maxSize, 23)

    context "with --qc-max-discard" $ do
      it "uses specified discard ratio to QuickCheck properties" $ do
        ["--qc-max-discard", "23"] `shouldUseArgs` (QC.maxDiscardRatio, 23)

    describe "Test.QuickCheck.Args.chatty" $ do
      let
        setChatty value args = args { chatty = value }
        withChatty value = hspecCapture [] .  H.modifyArgs (setChatty value) $ do
          H.it "foo" $ property True

      context "when True" $ do
        it "includes informational output" $ do
          withChatty True `shouldReturn` unlines [
              ""
            , "foo [✔]"
            , "  +++ OK, passed 1 test."
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures"
            ]

      context "when False" $ do
        it "suppresses informational output" $ do
          withChatty False `shouldReturn` unlines [
              ""
            , "foo [✔]"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures"
            ]

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
        r `shouldContain` "foo [<span class=\"hspec-success\">✔</span>]\n"

      it "marks pending examples with CSS class hspec-pending" $ do
        r <- capture_ . withArgs ["--html"] . H.hspec $ do
          H.it "foo" H.pending
        r `shouldContain` "foo [<span class=\"hspec-pending\">‐</span>]\n"

      it "marks failed examples with CSS class hspec-failure" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--html"] . H.hspec $ do
          H.it "foo" False
        r `shouldContain` "foo [<span class=\"hspec-failure\">✘</span>]\n"

  describe "hspecResult" $ do
    let
      hspecResult args = withArgs args . hspecResultSilent
      hspecResult_ = hspecResultSilent

    it "returns a summary of the test run" $ do
      hspecResult_ $ do
        H.it "foo" True
        H.it "foo" False
        H.it "foo" False
        H.it "foo" True
        H.it "foo" True
      `shouldReturn` H.Summary 5 2

    it "treats uncaught exceptions as failure" $ do
      hspecResult_  $ do
        H.it "foobar" throwException_
      `shouldReturn` H.Summary 1 1

    it "handles unguarded exceptions in runner" $ do
      let
        throwExceptionThatIsNotGuardedBy_safeTry :: H.Item () -> H.Item ()
        throwExceptionThatIsNotGuardedBy_safeTry item = item {
          H.itemExample = \ _params _hook _progress -> throwIO DivideByZero
        }
      hspecResult_ $ H.mapSpecItem_ throwExceptionThatIsNotGuardedBy_safeTry $ do
        H.it "foo" True
      `shouldReturn` H.Summary 1 1

    it "uses the specdoc formatter by default" $ do
      _:r:_ <- captureLines . H.hspecResult $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` "Foo.Bar"

    it "does not let escape error thunks from failure messages" $ do
      r <- hspecResult_ $ do
        H.it "some example" (H.Result "" $ H.Failure Nothing . H.Reason $ "foobar" ++ undefined)
      r `shouldBe` H.Summary 1 1

    it "runs specs in parallel" $ do
      let n = 100
          t = 0.01
          dt = t * (fromIntegral n / 2)
      r <- timeout dt . hspecResult ["-j", show n] . H.parallel $ do
        replicateM_ n (H.it "foo" $ sleep t)
      r `shouldBe` Just (H.Summary n 0)

    context "with -j" $ do
      it "limits parallelism" $ do
        currentRef <- newIORef (0 :: Int)
        highRef <- newIORef 0
        let n = 10
            t = 0.01
            j = 2
            start = do
              current <- atomicModifyIORef currentRef $ \x -> let y = succ x in (y, y)
              atomicModifyIORef highRef $ \x -> (max x current, ())
            stop = atomicModifyIORef currentRef $ \x -> (pred x, ())
        r <- hspecResult ["-j", show j] . H.parallel $ do
          replicateM_ n $ H.it "foo" $ bracket_ start stop $ sleep t
        r `shouldBe` H.Summary n 0
        high <- readIORef highRef
        high `shouldBe` j

  describe "colorOutputSupported" $ do
    context "without a terminal device" $ do

      let isTerminalDevice = return False

      it "disables color output" $ do
        H.colorOutputSupported H.ColorAuto isTerminalDevice `shouldReturn` ColorDisabled

      context "with GITHUB_ACTIONS=true" $ do
        it "enable color output" $ do
          withEnvironment [("GITHUB_ACTIONS", "true")] $ do
            H.colorOutputSupported H.ColorAuto isTerminalDevice `shouldReturn` ColorEnabled ProgressReportingDisabled

    context "with a terminal device" $ do

      let isTerminalDevice = return True

      it "enable color output" $ do
        H.colorOutputSupported H.ColorAuto isTerminalDevice `shouldReturn` ColorEnabled ProgressReportingEnabled

      context "with BUILDKITE=true" $ do
        it "disables progress reporting" $ do
          withEnvironment [("BUILDKITE", "true")] $ do
            H.colorOutputSupported H.ColorAuto isTerminalDevice `shouldReturn` ColorEnabled ProgressReportingDisabled

      context "when NO_COLOR is set" $ do
        it "disables color output" $ do
          withEnvironment [("NO_COLOR", "yes")] $ do
            H.colorOutputSupported H.ColorAuto isTerminalDevice `shouldReturn` ColorDisabled

  describe "unicodeOutputSupported" $ do
    context "with UnicodeAlways" $ do
      it "returns True" $ do
        H.unicodeOutputSupported H.UnicodeAlways undefined `shouldReturn` True

    context "with UnicodeNever" $ do
      it "returns False" $ do
        H.unicodeOutputSupported H.UnicodeNever undefined `shouldReturn` False

    context "with UnicodeAuto" $ do
      context "when file encoding is UTF-8" $ do
        it "returns True" $ do
          inTempDirectory $ do
            withFile "foo" WriteMode $ \ h -> do
              hSetEncoding h utf8
              H.unicodeOutputSupported H.UnicodeAuto h `shouldReturn` True

      context "when file encoding is not UTF-8" $ do
        it "returns False" $ do
          inTempDirectory $ do
            withFile "foo" WriteMode $ \ h -> do
              hSetEncoding h latin1
              H.unicodeOutputSupported H.UnicodeAuto h `shouldReturn` False

  describe "rerunAll" $ do
    let
      report = FailureReport 0 0 0 0 [([], "foo")]
      config = H.defaultConfig {H.configRerun = True, H.configRerunAllOnSuccess = True}
      result = SpecResult [] True
    context "with --rerun, --rerun-all-on-success, previous failures, on success" $ do
      it "returns True" $ do
        H.rerunAll config (Just report) result `shouldBe` True

    context "without --rerun" $ do
      it "returns False" $ do
        H.rerunAll config {H.configRerun = False} (Just report) result `shouldBe` False

    context "without --rerun-all-on-success" $ do
      it "returns False" $ do
        H.rerunAll config {H.configRerunAllOnSuccess = False} (Just report) result `shouldBe` False

    context "without previous failures" $ do
      it "returns False" $ do
        H.rerunAll config (Just report {failureReportPaths = []}) result `shouldBe` False

    context "without failure report" $ do
      it "returns False" $ do
        H.rerunAll config Nothing result `shouldBe` False

    context "on failure" $ do
      it "returns False" $ do
        H.rerunAll config (Just report) result { specResultSuccess = False } `shouldBe` False
