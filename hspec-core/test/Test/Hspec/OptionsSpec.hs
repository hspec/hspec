module Test.Hspec.OptionsSpec (spec) where

import           Control.Monad
import           Helper
import           System.Exit

import qualified Test.Hspec.Options as Options
import           Test.Hspec.Options hiding (parseOptions)

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: No left value!"

spec :: Spec
spec = do
  describe "parseOptions" $ do

    let parseOptions = Options.parseOptions defaultConfig "my-spec"

    it "rejects unexpected arguments" $ do
      fromLeft (parseOptions [] ["foo"]) `shouldBe` (ExitFailure 1, "my-spec: unexpected argument `foo'\nTry `my-spec --help' for more information.\n")

    it "rejects unrecognized options" $ do
      fromLeft (parseOptions [] ["--foo"]) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--foo'\nTry `my-spec --help' for more information.\n")

    it "sets configColorMode to ColorAuto" $ do
      configColorMode <$> parseOptions [] [] `shouldBe` Right ColorAuto

    context "with --no-color" $ do
      it "sets configColorMode to ColorNever" $ do
        configColorMode <$> parseOptions [] ["--no-color"] `shouldBe` Right ColorNever

    context "with --color" $ do
      it "sets configColorMode to ColorAlways" $ do
        configColorMode <$> parseOptions [] ["--color"] `shouldBe` Right ColorAlways

    context "with --out" $ do
      it "sets configOutputFile" $ do
        either (const Nothing) Just . configOutputFile <$> parseOptions [] ["--out", "foo"] `shouldBe` Right (Just "foo")

    context "with --qc-max-success" $ do
      context "when given an invalid argument" $ do
        it "returns an error message" $ do
          fromLeft (parseOptions [] ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success'\nTry `my-spec --help' for more information.\n")

    context "with --depth" $ do
      it "sets depth parameter for SmallCheck" $ do
        configSmallCheckDepth <$> parseOptions [] ["--depth", "23"] `shouldBe` Right 23

    context "with --jobs" $ do
      it "sets number of concurrent jobs" $ do
        configConcurrentJobs <$> parseOptions [] ["--jobs=23"] `shouldBe` Right (Just 23)

      it "rejects values < 1" $ do
        let msg = "my-spec: invalid argument `0' for `--jobs'\nTry `my-spec --help' for more information.\n"
        void (parseOptions [] ["--jobs=0"]) `shouldBe` Left (ExitFailure 1, msg)

    context "when given a config file" $ do
      it "uses options from config file" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] ["--color"] `shouldBe` Right ColorAlways

      it "rejects --help" $ do
        fromLeft (parseOptions [("~/.hspec", ["--help"])] []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--help' in config file ~/.hspec\n")

      it "rejects unrecognized options" $ do
        fromLeft (parseOptions [("~/.hspec", ["--invalid"])] []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--invalid' in config file ~/.hspec\n")

    context "when given multiple config files" $ do
      it "gives later config files precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"]), (".hspec", ["--color"])] [] `shouldBe` Right ColorAlways

  describe "ignoreConfigFile" $ around_ (withEnvironment []) $ do
    context "by default" $ do
      it "returns False" $ do
        ignoreConfigFile defaultConfig [] `shouldReturn` False

    context "with --ignore-dot-hspec" $ do
      it "returns True" $ do
        ignoreConfigFile defaultConfig ["--ignore-dot-hspec"] `shouldReturn` True

    context "with IGNORE_DOT_HSPEC" $ do
      it "returns True" $ do
        withEnvironment [("IGNORE_DOT_HSPEC", "yes")] $ do
          ignoreConfigFile defaultConfig [] `shouldReturn` True
