module Test.Hspec.Core.Config.OptionsSpec (spec) where

import           Prelude ()
import           Helper

import           System.Exit

import qualified Test.Hspec.Core.Config.Options as Options
import           Test.Hspec.Core.Config.Options hiding (parseOptions)

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: No left value!"

spec :: Spec
spec = do
  describe "parseOptions" $ do

    let parseOptions = Options.parseOptions defaultConfig "my-spec"

    it "rejects unexpected arguments" $ do
      fromLeft (parseOptions [] Nothing ["foo"]) `shouldBe` (ExitFailure 1, "my-spec: unexpected argument `foo'\nTry `my-spec --help' for more information.\n")

    it "rejects unrecognized options" $ do
      fromLeft (parseOptions [] Nothing ["--foo"]) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--foo'\nTry `my-spec --help' for more information.\n")

    it "sets configColorMode to ColorAuto" $ do
      configColorMode <$> parseOptions [] Nothing [] `shouldBe` Right ColorAuto

    context "with --help" $ do
      let Left (code, output) = parseOptions [] Nothing ["--help"]
          help = lines output

      it "returns ExitSuccess" $ do
        code `shouldBe` ExitSuccess

      it "prints help" $ do
        help `shouldStartWith` ["Usage: my-spec [OPTION]..."]

    context "with --no-color" $ do
      it "sets configColorMode to ColorNever" $ do
        configColorMode <$> parseOptions [] Nothing ["--no-color"] `shouldBe` Right ColorNever

    context "with --color" $ do
      it "sets configColorMode to ColorAlways" $ do
        configColorMode <$> parseOptions [] Nothing ["--color"] `shouldBe` Right ColorAlways

    context "with --diff" $ do
      it "sets configDiff to True" $ do
        configDiff <$> parseOptions [] Nothing ["--diff"] `shouldBe` Right True

    context "with --no-diff" $ do
      it "sets configDiff to False" $ do
        configDiff <$> parseOptions [] Nothing ["--no-diff"] `shouldBe` Right False

    context "with --out" $ do
      it "sets configOutputFile" $ do
        either (const Nothing) Just . configOutputFile <$> parseOptions [] Nothing ["--out", "foo"] `shouldBe` Right (Just "foo")

    context "with --qc-max-success" $ do
      context "when given an invalid argument" $ do
        it "returns an error message" $ do
          fromLeft (parseOptions [] Nothing ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success'\nTry `my-spec --help' for more information.\n")

    context "with --depth" $ do
      it "sets depth parameter for SmallCheck" $ do
        configSmallCheckDepth <$> parseOptions [] Nothing ["--depth", "23"] `shouldBe` Right 23

    context "with --jobs" $ do
      it "sets number of concurrent jobs" $ do
        configConcurrentJobs <$> parseOptions [] Nothing ["--jobs=23"] `shouldBe` Right (Just 23)

      it "rejects values < 1" $ do
        let msg = "my-spec: invalid argument `0' for `--jobs'\nTry `my-spec --help' for more information.\n"
        void (parseOptions [] Nothing ["--jobs=0"]) `shouldBe` Left (ExitFailure 1, msg)

    context "when given a config file" $ do
      it "uses options from config file" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] Nothing [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] Nothing ["--color"] `shouldBe` Right ColorAlways

      it "rejects --help" $ do
        fromLeft (parseOptions [("~/.hspec", ["--help"])] Nothing []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--help' in config file ~/.hspec\n")

      it "rejects unrecognized options" $ do
        fromLeft (parseOptions [("~/.hspec", ["--invalid"])] Nothing []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--invalid' in config file ~/.hspec\n")

      it "rejects ambiguous options" $ do
        fromLeft (parseOptions [("~/.hspec", ["--fail"])] Nothing []) `shouldBe` (ExitFailure 1,
          unlines [
            "my-spec: option `--fail' is ambiguous; could be one of:"
          , "    --fail-on-focused      fail on focused spec items"
          , "    --fail-fast            abort on first failure"
          , "    --failure-report=FILE  read/write a failure report for use with --rerun"
          , "in config file ~/.hspec"
          ]
          )

    context "when given multiple config files" $ do
      it "gives later config files precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"]), (".hspec", ["--color"])] Nothing [] `shouldBe` Right ColorAlways

    context "when given an environment variable" $ do
      it "uses options from environment variable" $ do
        configColorMode <$> parseOptions [] (Just ["--no-color"]) [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [] (Just ["--no-color"]) ["--color"] `shouldBe` Right ColorAlways

      it "rejects unrecognized options" $ do
        fromLeft (parseOptions [] (Just ["--invalid"]) []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--invalid' from environment variable HSPEC_OPTIONS\n")

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
