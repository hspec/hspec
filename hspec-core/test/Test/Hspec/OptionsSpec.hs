module Test.Hspec.OptionsSpec (main, spec) where

import           Control.Monad
import           Helper
import           System.Exit

import qualified Test.Hspec.Options as Options
import           Test.Hspec.Options hiding (parseOptions)

main :: IO ()
main = hspec spec

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: No left value!"

spec :: Spec
spec = do
  describe "parseOptions" $ do

    let parseOps = Options.parseOptions defaultConfig "my-spec"
        parseOptions = parseOps ""

    it "sets configColorMode to ColorAuto" $ do
      configColorMode <$> parseOptions [] `shouldBe` Right ColorAuto

    context "with --no-color" $ do
      it "sets configColorMode to ColorNever" $ do
        configColorMode <$> parseOptions ["--no-color"] `shouldBe` Right ColorNever

    context "with --color" $ do
      it "sets configColorMode to ColorAlways" $ do
        configColorMode <$> parseOptions ["--color"] `shouldBe` Right ColorAlways

    context "with --out" $ do
      it "sets configOutputFile" $ do
        either (const Nothing) Just . configOutputFile <$> parseOptions ["--out", "foo"] `shouldBe` Right (Just "foo")

    context "with --qc-max-success" $ do
      context "when given an invalid argument" $ do
        it "returns an error message" $ do
          fromLeft (parseOptions ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success'\nTry `my-spec --help' for more information.\n")

      context "appends source to error message" $ do
        it "returns an error message with source specified" $ do
          fromLeft (parseOps " (defined in ./.hspec)" ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success' (defined in ./.hspec)\nTry `my-spec --help' for more information.\n")

    context "with --depth" $ do
      it "sets depth parameter for SmallCheck" $ do
        configSmallCheckDepth <$> parseOptions ["--depth", "23"] `shouldBe` Right 23

    context "with --jobs" $ do
      it "sets number of concurrent jobs" $ do
        configConcurrentJobs <$> parseOptions ["--jobs=23"] `shouldBe` Right (Just 23)

      it "rejects values < 1" $ do
        let msg = "my-spec: invalid argument `0' for `--jobs'\nTry `my-spec --help' for more information.\n"
        void (parseOptions ["--jobs=0"]) `shouldBe` Left (ExitFailure 1, msg)
