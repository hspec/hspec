module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.Meta
import           System.IO.Silently (hCapture, hSilence)
import           System.IO (stderr)

import           Control.Applicative
import           System.Environment (withArgs, withProgName, getArgs)
import           System.Exit
import qualified Control.Exception as E
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Monadic as H (describe, it)
import           Test.Hspec.Monadic (runSpecM)
import qualified Test.Hspec.Formatters as H
import           Util (capture)

import           Mock

import           System.SetEnv
import           Test.Hspec.Util (getEnv)

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
      r <- capture . H.hspec $ do
        H.it "foobar" $ do
          putStrLn "baz"
      r `shouldSatisfy` notElem "baz"

    context "command-line options" $ do
      it "prints an error message on unrecognized options" $ do
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

      describe "option '--verbose'" $ do
        it "does not suppress output to stdout when evaluating examples" $ do
          r <- capture . withArgs ["--verbose"] .  H.hspec $ do
            H.it "foobar" $ do
              putStrLn "baz"
          r `shouldSatisfy` elem "baz"

      describe "option '--match'" $ do
        it "only runs examples that match a given pattern" $ do
          e1 <- newMock
          e2 <- newMock
          e3 <- newMock
          withArgs ["-m", "/bar/example"] .  H.hspec $ do
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
          withArgs ["-m", "foo", "-m", "baz"] .  H.hspec $ do
            H.describe "foo" $ do
              H.it "example 1" $ mockAction e1
            H.describe "bar" $ do
              H.it "example 2" $ mockAction e2
            H.describe "baz" $ do
              H.it "example 3" $ mockAction e3
          (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 0, 1)

    describe "experimental features" $ do
      it "keeps a failure report in the environment" $ do
        ignoreExitCode . H.hspec $ do
          H.describe "foo" $ do
            H.describe "bar" $ do
              H.it "example 1" True
              H.it "example 2" False
          H.describe "baz" $ do
            H.it "example 3" False
        getEnv "HSPEC_FAILURES" `shouldReturn` Just "[([\"foo\",\"bar\"],\"example 2\"),([\"baz\"],\"example 3\")]"

      describe "option '--re-run'" $ do
        let runSpec = (capture . ignoreExitCode . H.hspec) $ do
              H.it "example 1" True
              H.it "example 2" False
              H.it "example 3" False
              H.it "example 4" True
              H.it "example 5" False

        it "re-runs examples that previously failed" $ do
          r0 <- runSpec
          r0 `shouldSatisfy` elem "5 examples, 0 pending, 3 failures"

          r1 <- withArgs ["-r"] runSpec
          r1 `shouldSatisfy` elem "3 examples, 0 pending, 3 failures"

        context "when there is no failure report in the environment" $ do
          it "runs everything" $ do
            unsetEnv "HSPEC_FAILURES"
            r <- hSilence [stderr] $ withArgs ["-r"] runSpec
            r `shouldSatisfy` elem "5 examples, 0 pending, 3 failures"

          it "prints a warning to stderr" $ do
            unsetEnv "HSPEC_FAILURES"
            r <- hCapture [stderr] $ withArgs ["-r"] runSpec
            fst r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--re-run' is ignored!\n"

        context "when parsing of failure report fails" $ do
          it "runs everything" $ do
            setEnv "HSPEC_FAILURES" "some invalid report"
            r <- hSilence [stderr] $ withArgs ["-r"] runSpec
            r `shouldSatisfy` elem "5 examples, 0 pending, 3 failures"

          it "prints a warning to stderr" $ do
            setEnv "HSPEC_FAILURES" "some invalid report"
            r <- hCapture [stderr] $ withArgs ["-r"] runSpec
            fst r `shouldBe` "WARNING: Could not read environment variable HSPEC_FAILURES; `--re-run' is ignored!\n"

  describe "hspecWith" $ do
    it "returns a summary of the test run" $ do
      H.hspecWith H.defaultConfig . runSpecM $ do
        H.it "foo" True
        H.it "foo" False
        H.it "foo" False
        H.it "foo" True
        H.it "foo" True
      `shouldReturn` H.Summary 5 2

    it "uses the specdoc formatter by default" $ do
      let testSpec = (runSpecM $ H.describe "Foo.Bar" $ H.it "some example" True)
      _:r:_ <- capture $ H.hspecWith H.defaultConfig testSpec
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      let testSpec = (runSpecM $ H.describe "Foo.Bar" $ return ())
      [] <- capture $ H.hspecWith H.defaultConfig {H.configFormatter = H.silent} testSpec
      return ()
