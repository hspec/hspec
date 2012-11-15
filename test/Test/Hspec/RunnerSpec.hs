module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.Meta
import           System.IO.Silently (hCapture, hSilence)
import           System.IO (stderr)
import           Control.Applicative
import           System.Environment (withArgs, withProgName, getArgs)
import           System.Exit
import qualified Control.Exception as E
import           Util
import           Mock
import           System.SetEnv
import           Test.Hspec.Util (getEnv)
import           Test.QuickCheck.Property (morallyDubiousIOProperty)

import qualified Test.Hspec.Runner as H
import qualified Test.Hspec as H (describe, it, pending)
import qualified Test.Hspec.Formatters as H (silent)

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` \e -> let _ = e :: ExitCode in return ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hspec" $ do
    it "runs a spec" $ do
      H.hspec $ do
        H.it "foobar" True
      `shouldReturn` ()

    it "exits with exitFailure if not all examples pass" $ do
      H.hspec $ do
        H.it "foobar" False
      `shouldThrow` (== ExitFailure 1)

    it "suppresses output to stdout when evaluating examples" $ do
      r <- capture__ . H.hspec $ do
        H.it "foobar" $ do
          putStrLn "baz"
      r `shouldSatisfy` notElem "baz"

    it "prints an error message on unrecognized command-line options" $ do
      withProgName "myspec" . withArgs ["--foo"] $ do
        hSilence [stderr] (H.hspec $ pure ()) `shouldThrow` (== ExitFailure 1)
        fst `fmap` hCapture [stderr] (ignoreExitCode (H.hspec $ pure ())) `shouldReturn` unlines [
            "myspec: unrecognized option `--foo'"
          , "Try `myspec --help' for more information."
          ]

    it "does not leak command-line flags to examples" $ do
      withArgs ["--verbose"] $ do
        H.hspec $ do
          H.it "foobar" $ do
            getArgs `shouldReturn` []
        `shouldReturn` ()

    context "with --help" $ do
      let printHelp = withProgName "spec" . withArgs ["--help"] . H.hspec $ pure ()
      it "prints help" $ do
        r <- (capture__ . ignoreExitCode) printHelp
        r `shouldStartWith` ["Usage: spec [OPTION]..."]
        printHelp `shouldThrow` (== ExitSuccess)

      it "constrains lines to 80 characters" $ do
        r <- (capture__ . ignoreExitCode) printHelp
        r `shouldSatisfy` all ((<= 80) . length)
        r `shouldSatisfy` any ((78 <=) . length)

    context "with --verbose" $ do
      it "does not suppress output to stdout" $ do
        r <- capture__ . withArgs ["--verbose"] . H.hspec $ do
          H.it "foobar" $ do
            putStrLn "baz"
        r `shouldSatisfy` elem "baz"

    context "with --match" $ do
      it "only runs examples that match a given pattern" $ do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        withArgs ["-m", "/bar/example"] . H.hspec $ do
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
        withArgs ["-m", "foo", "-m", "baz"] . H.hspec $ do
          H.describe "foo" $ do
            H.it "example 1" $ mockAction e1
          H.describe "bar" $ do
            H.it "example 2" $ mockAction e2
          H.describe "baz" $ do
            H.it "example 3" $ mockAction e3
        (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 0, 1)

    context "with --maximum-generated-tests=n" $ do
      it "tries QuickCheck properties n times" $ do
        m <- newMock
        withArgs ["--maximum-generated-tests=23"] . H.hspec $ do
          H.it "foo" $ do
            morallyDubiousIOProperty (mockAction m >> pure True)
        mockCounter m `shouldReturn` 23

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

  describe "hspec (experimental features)" $ do
    it "stores a failure report in environment HSPEC_FAILURES" $ do
      ignoreExitCode . H.hspec $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "example 1" True
            H.it "example 2" False
        H.describe "baz" $ do
          H.it "example 3" False
      getEnv "HSPEC_FAILURES" `shouldReturn` Just "[([\"foo\",\"bar\"],\"example 2\"),([\"baz\"],\"example 3\")]"

    describe "with --re-run" $ do
      let runSpec = (capture__ . ignoreExitCode . H.hspec) $ do
            H.it "example 1" True
            H.it "example 2" False
            H.it "example 3" False
            H.it "example 4" True
            H.it "example 5" False

      it "re-runs examples that previously failed" $ do
        r0 <- runSpec
        r0 `shouldSatisfy` elem "5 examples, 3 failures"

        r1 <- withArgs ["-r"] runSpec
        r1 `shouldSatisfy` elem "3 examples, 3 failures"

      context "when there is no failure report in the environment" $ do
        it "runs everything" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hSilence [stderr] $ withArgs ["-r"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          unsetEnv "HSPEC_FAILURES"
          r <- hCapture [stderr] $ withArgs ["-r"] runSpec
          fst r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--re-run' is ignored!\n"

      context "when parsing of failure report fails" $ do
        it "runs everything" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hSilence [stderr] $ withArgs ["-r"] runSpec
          r `shouldSatisfy` elem "5 examples, 3 failures"

        it "prints a warning to stderr" $ do
          setEnv "HSPEC_FAILURES" "some invalid report"
          r <- hCapture [stderr] $ withArgs ["-r"] runSpec
          fst r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--re-run' is ignored!\n"

  describe "hspecWith" $ do
    it "returns a summary of the test run" $ do
      H.hspecWith H.defaultConfig $ do
        H.it "foo" True
        H.it "foo" False
        H.it "foo" False
        H.it "foo" True
        H.it "foo" True
      `shouldReturn` H.Summary 5 2

    it "treats uncaught exceptions as failure" $ do
      H.hspecWith H.defaultConfig  $ do
        H.it "foobar" (E.throwIO (E.ErrorCall "foobar") >> pure ())
      `shouldReturn` H.Summary 1 1

    it "uses the specdoc formatter by default" $ do
      _:r:_ <- capture__ . H.hspecWith H.defaultConfig $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      r <- capture_ . H.hspecWith H.defaultConfig {H.configFormatter = H.silent} $ do
        H.describe "Foo.Bar" $ do
          H.it "some example" True
      r `shouldBe` ""
