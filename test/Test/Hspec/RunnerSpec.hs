module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.Meta
import           System.IO.Silently (hCapture, hSilence)
import           System.IO (stderr)

import           Control.Applicative
import           System.Environment
import           System.Exit
import qualified Control.Exception as E
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Formatters as H
import           Util

import           Mock

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` ignore
  where
    ignore :: ExitCode -> IO ()
    ignore _ = return ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hspec" $ do
    it "runs a spec" $ do
      H.hspec [H.it "foobar" True] `shouldReturn` ()

    it "exits with exitFailure if not all examples pass" $ do
      H.hspec [H.it "foobar" False] `shouldThrow` (== ExitFailure 1)

    it "suppresses output to stdout when evaluating examples" $ do
      r <- capture $
        H.hspec [H.it "foobar" $ putStrLn "baz"]
      r `shouldSatisfy` notElem "baz"

    context "command-line options" $ do
      it "prints error message on unrecognized option" $ do
        withProgName "myspec" . withArgs ["--foo"] $ do
          hSilence [stderr] (H.hspec []) `shouldThrow` (== ExitFailure 1)
          fst `fmap` hCapture [stderr] (ignoreExitCode (H.hspec [])) `shouldReturn` unlines [
              "myspec: unrecognized option `--foo'"
            , "Try `myspec --help' for more information."
            ]

      it "does not leak command-line flags to examples" $ do
        withArgs ["--verbose"] $
          H.hspec [H.it "foobar" $ getArgs `shouldReturn` []] `shouldReturn` ()

      describe "option '--verbose'" $ do
        it "does not suppress output to stdout when evaluating examples" $ do
          r <- capture . withArgs ["--verbose"] $
            H.hspec [H.it "foobar" $ putStrLn "baz"]
          r `shouldSatisfy` elem "baz"

      describe "option '--match'" $ do
        it "only runs examples that match a given pattern" $ do
          e1 <- newMock
          e2 <- newMock
          e3 <- newMock
          withArgs ["-m", "/bar/example"] $
            H.hspec [
              H.describe "foo" [
                H.describe "bar" [
                  H.it "example 1" $ mockAction e1
                , H.it "example 2" $ mockAction e2
                ]
              , H.describe "baz" [
                  H.it "example 3" $ mockAction e3
                ]
              ]
            ]
          (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 1, 0)

        it "can be given multiple times" $ do
          e1 <- newMock
          e2 <- newMock
          e3 <- newMock
          withArgs ["-m", "foo", "-m", "baz"] $
            H.hspec [
              H.describe "foo" [
                H.it "example 1" $ mockAction e1
              ]
            , H.describe "bar" [
                H.it "example 2" $ mockAction e2
              ]
            , H.describe "baz" [
                H.it "example 3" $ mockAction e3
              ]
            ]
          (,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 `shouldReturn` (1, 0, 1)

  describe "hspecWith" $ do
    it "returns a summary of the test run" $ do
      let testSpec = [
              H.it "foo" True
            , H.it "foo" False
            , H.it "foo" False
            , H.it "foo" True
            , H.it "foo" True
            ]
      H.hspecWith H.defaultConfig testSpec `shouldReturn` H.Summary 5 2

    it "uses the specdoc formatter by default" $ do
      let testSpec = [H.describe "Foo.Bar" [H.it "some example" True]]
      _:r:_ <- capture $ H.hspecWith H.defaultConfig testSpec
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      let testSpec = [H.describe "Foo.Bar" []]
      [] <- capture $ H.hspecWith H.defaultConfig {H.configFormatter = H.silent} testSpec
      return ()
