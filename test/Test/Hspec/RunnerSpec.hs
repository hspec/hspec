module Test.Hspec.RunnerSpec (main, spec) where

import           Test.Hspec.Meta

import           System.Environment
import           System.Exit
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Formatters as H
import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hspec" $ do
    it "runs a spec" $ do
      H.hspec [H.it "foobar" True] `shouldReturn` ()

    it "exits with exitFailure if not all examples pass" $ do
      H.hspec [H.it "foobar" False] `shouldThrow` (== ExitFailure 1)

    it "does not leak command-line flags to examples" $ do
      withArgs ["--verbose"] $
        H.hspec [H.it "foobar" $ getArgs `shouldReturn` []] `shouldReturn` ()

    it "suppresses output to stdout when evaluating examples" $ do
      r <- capture $
        H.hspec [H.it "foobar" $ putStrLn "baz"]
      r `shouldSatisfy` notElem "baz"

    describe "command-line flag '--verbose'" $ do
      it "does not suppress output to stdout when evaluating examples" $ do
        r <- capture . withArgs ["--verbose"] $
          H.hspec [H.it "foobar" $ putStrLn "baz"]
        r `shouldSatisfy` elem "baz"

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
      let testSpec = [H.describe "Foo.Bar" []]
      _:r:_ <- capture $ H.hspecWith H.defaultConfig testSpec
      r `shouldBe` "Foo.Bar"

    it "can use a custom formatter" $ do
      let testSpec = [H.describe "Foo.Bar" []]
      [] <- capture $ H.hspecWith H.defaultConfig {H.configFormatter = H.silent} testSpec
      return ()
