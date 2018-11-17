{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.RunnerSpec (spec) where

import           Prelude ()
import           Helper

import           System.IO (stderr)
import           System.Environment (withArgs, withProgName, getArgs)
import           System.Exit
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Concurrent.Async
import           Mock
import           System.SetEnv
import           System.Console.ANSI

import           Test.Hspec.Core.FailureReport (FailureReport(..))
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Formatters as H (silent)
import qualified Test.Hspec.Core.QuickCheck as H

import qualified Test.QuickCheck as QC
import qualified Test.Hspec.Core.Hooks as H

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

    let
      hspec args = withArgs ("--format=silent" : args) . H.hspec
      hspec_ = hspec []

    it "evaluates examples Unmasked" $ do
      mvar <- newEmptyMVar
      hspec_ $ do
        H.it "foo" $ do
          E.getMaskingState >>= putMVar mvar
      takeMVar mvar `shouldReturn` E.Unmasked

    it "runs finalizers" $ do
      mvar <- newEmptyMVar
      ref <- newIORef "did not run finalizer"
      a <- async $ hspec_ $ do
          H.it "foo" $ do
            (putMVar mvar () >> threadDelay 10000000) `E.finally`
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
        runPropFoo ["--rerun"] `shouldReturn` r

      forM_ quickCheckOptions $ \(name, accessor) -> do
        it ("reuses same " ++ name) $ do
          [name, "23"] `shouldUseArgs` ((== 23) . accessor)
          ["--rerun"] `shouldUseArgs` ((== 23) . accessor)

      context "when no examples failed previously" $ do
        it "runs all examples" $ do
          let run = capture_ . H.hspec $ do
                H.it "example 1" True
                H.it "example 2" True
                H.it "example 3" True

          r0 <- run
          r0 `shouldContain` "3 examples, 0 failures"

          r1 <- withArgs ["--rerun"] run
          r1 `shouldContain` "3 examples, 0 failures"

      context "when there is no failure report in the environment" $ do
        it "runs everything" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hSilence [stderr] $ withArgs ["--rerun"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hCapture_ [stderr] $ withArgs ["--rerun"] runSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"

      context "when parsing of failure report fails" $ do
        it "runs everything" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hSilence [stderr] $ withArgs ["--rerun"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hCapture_ [stderr] $ withArgs ["--rerun"] runSpec
          r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!\n"


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
        throwTo threadId E.UserInterrupt
        r <- takeMVar mvar
        normalizeSummary r `shouldBe` [
            ""
          , "foo FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  1) foo"
          , ""
          , "  To rerun use: --match \"/foo/\""
          , ""
#if __GLASGOW_HASKELL__ == 800
          , "WARNING:"
          , "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
          , "  Source locations may not work as expected."
          , ""
          , "  Please consider upgrading GHC!"
          , ""
#endif
          , "Randomized with seed 23"
          , ""
          ]

      it "throws UserInterrupt" $ do
        mvar <- newEmptyMVar
        sync <- newEmptyMVar
        threadId <- forkIO $ do
          hspec_ $ do
            H.it "foo" $ do
              putMVar sync ()
              threadDelay 1000000
          `E.catch` putMVar mvar
        takeMVar sync
        throwTo threadId E.UserInterrupt
        takeMVar mvar `shouldReturn` E.UserInterrupt

    context "with --dry-run" $ do
      let withDryRun = captureLines . withArgs ["--dry-run"] . H.hspec
      it "produces a report" $ do
        r <- withDryRun $ do
          H.it "foo" True
          H.it "bar" True
        normalizeSummary r `shouldBe` [
            ""
          , "foo"
          , "bar"
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
      let run = captureLines . withArgs ["--focused-only"] . H.hspec
      context "when there aren't any focused spec items" $ do
        it "does not run anything" $ do
          r <- run $ do
            H.it "foo" True
            H.it "bar" True
          normalizeSummary r `shouldBe` [
              ""
            , ""
            , "Finished in 0.0000 seconds"
            , "0 examples, 0 failures"
            ]

    context "with --fail-on-focused" $ do
      let run = captureLines . ignoreExitCode . withArgs ["--fail-on-focused", "--seed", "23"] . H.hspec . removeLocations
      it "fails on focused spec items" $ do
        r <- run $ do
          H.it "foo" True
          H.fit "bar" True
        normalizeSummary r `shouldBe` [
            ""
          , "bar FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  1) bar"
          , "       item is focused; failing due to --fail-on-focused"
          , ""
          , "  To rerun use: --match \"/bar/\""
          , ""
          , "Randomized with seed 23"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

    context "with --fail-fast" $ do
      it "stops after first failure" $ do
        r <- captureLines . ignoreExitCode . withArgs ["--fail-fast", "--seed", "23"] . H.hspec . removeLocations $ do
          H.it "foo" True
          H.it "bar" False
          H.it "baz" False
        normalizeSummary r `shouldBe` [
            ""
          , "foo"
          , "bar FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  1) bar"
          , ""
          , "  To rerun use: --match \"/bar/\""
          , ""
#if __GLASGOW_HASKELL__ == 800
          , "WARNING:"
          , "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
          , "  Source locations may not work as expected."
          , ""
          , "  Please consider upgrading GHC!"
          , ""
#endif
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
              (signalQSem child1 >> waitQSem child2 >> writeIORef ref "foo") `E.finally` signalQSem parent
        signalQSem child2
        waitQSem parent
        readIORef ref `shouldReturn` ""

      it "works for nested specs" $ do
        r <- captureLines . ignoreExitCode . withArgs ["--fail-fast", "--seed", "23"] . H.hspec . removeLocations $ do
          H.describe "foo" $ do
            H.it "bar" False
            H.it "baz" True
        normalizeSummary r `shouldBe` [
            ""
          , "foo"
          , "  bar FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  1) foo bar"
          , ""
          , "  To rerun use: --match \"/foo/bar/\""
          , ""
#if __GLASGOW_HASKELL__ == 800
          , "WARNING:"
          , "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
          , "  Source locations may not work as expected."
          , ""
          , "  Please consider upgrading GHC!"
          , ""
#endif
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

    context "with --diff" $ do
      it "shows colorized diffs" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--diff", "--color"] . H.hspec $ do
          H.it "foo" $ do
            23 `shouldBe` (42 :: Int)
        r `shouldContain` unlines [
            red ++ "       expected: " ++ reset ++ red ++ "42" ++ reset
          , red ++ "        but got: " ++ reset ++ green ++ "23" ++ reset
          ]

    context "with --no-diff" $ do
      it "it does not show colorized diffs" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--no-diff", "--color"] . H.hspec $ do
          H.it "foo" $ do
            23 `shouldBe` (42 :: Int)
        r `shouldContain` unlines [
            red ++ "       expected: " ++ reset ++ "42"
          , red ++ "        but got: " ++ reset ++ "23"
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
      it "tries QuickCheck properties specified number of times" $ do
        m <- newMock
        hspec ["--qc-max-success", "23"] $ do
          H.it "foo" $ property $ \(_ :: Int) -> do
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
        r `shouldContain` "<span class=\"hspec-success\">foo\n</span>"

      it "marks pending examples with CSS class hspec-pending" $ do
        r <- capture_ . withArgs ["--html"] . H.hspec $ do
          H.it "foo" H.pending
        r `shouldContain` "<span class=\"hspec-pending\">foo"

      it "marks failed examples with CSS class hspec-failure" $ do
        r <- capture_ . ignoreExitCode . withArgs ["--html"] . H.hspec $ do
          H.it "foo" False
        r `shouldContain` "<span class=\"hspec-failure\">foo"

  describe "hspecResult" $ do
    let
      hspecResult args = withArgs ("--format=silent" : args) . H.hspecResult
      hspecResult_ = hspecResult []

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
          replicateM_ n $ H.it "foo" $ E.bracket_ start stop $ sleep t
        r `shouldBe` H.Summary n 0
        high <- readIORef highRef
        high `shouldBe` j

  describe "rerunAll" $ do
    let
      report = FailureReport 0 0 0 0 [([], "foo")]
      config = H.defaultConfig {H.configRerun = True, H.configRerunAllOnSuccess = True}
      summary = H.Summary 1 0
    context "with --rerun, --rerun-all-on-success, previous failures, on success" $ do
      it "returns True" $ do
        H.rerunAll config (Just report) summary `shouldBe` True

    context "without --rerun" $ do
      it "returns False" $ do
        H.rerunAll config {H.configRerun = False} (Just report) summary `shouldBe` False

    context "without --rerun-all-on-success" $ do
      it "returns False" $ do
        H.rerunAll config {H.configRerunAllOnSuccess = False} (Just report) summary `shouldBe` False

    context "without previous failures" $ do
      it "returns False" $ do
        H.rerunAll config (Just report {failureReportPaths = []}) summary `shouldBe` False

    context "without failure report" $ do
      it "returns False" $ do
        H.rerunAll config Nothing summary `shouldBe` False

    context "on failure" $ do
      it "returns False" $ do
        H.rerunAll config (Just report) summary {H.summaryFailures = 1} `shouldBe` False
  where
    green  = setSGRCode [SetColor Foreground Dull Green]
    red    = setSGRCode [SetColor Foreground Dull Red]
    reset  = setSGRCode [Reset]
