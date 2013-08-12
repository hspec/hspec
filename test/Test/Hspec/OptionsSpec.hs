module Test.Hspec.OptionsSpec (main, spec) where

import           Helper
import           System.Exit

import           Test.Hspec.Options hiding (parseOptions)
import qualified Test.Hspec.Options as Options

main :: IO ()
main = hspec spec

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: No left value!"

spec :: Spec
spec = do
  describe "parseOptions" $ do

    let parseOptions = Options.parseOptions defaultOptions "my-spec"

    it "sets optionsColorMode to ColorAuto" $ do
      optionsColorMode <$> parseOptions [] `shouldBe` Right ColorAuto

    context "with --no-color" $ do
      it "sets optionsColorMode to ColorNever" $ do
        optionsColorMode <$> parseOptions ["--no-color"] `shouldBe` Right ColorNever

    context "with --color" $ do
      it "sets optionsColorMode to ColorAlways" $ do
        optionsColorMode <$> parseOptions ["--color"] `shouldBe` Right ColorAlways

    context "with --out" $ do
      it "sets optionsOutputFile" $ do
        optionsOutputFile <$> parseOptions ["--out", "foo"] `shouldBe` Right (Just "foo")

    context "with --qc-max-success" $ do
      context "when given an invalid argument" $ do
        it "returns an error message" $ do
          fromLeft (parseOptions ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success'\nTry `my-spec --help' for more information.\n")
