module Test.Hspec.Core.Config.OptionsSpec (spec) where

import           Prelude ()
import           Helper

import           System.Exit

import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Config.Options hiding (parseOptions)
import qualified Test.Hspec.Core.Config.Options as Options

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: No left value!"

spec :: Spec
spec = do
  describe "parseOptions" $ do

    let parseOptions configFiles envVar env args = snd <$> Options.parseOptions defaultConfig "my-spec" configFiles envVar env args

    it "rejects unexpected arguments" $ do
      fromLeft (parseOptions [] Nothing [] ["foo"]) `shouldBe` (ExitFailure 1, "my-spec: unexpected argument `foo'\nTry `my-spec --help' for more information.\n")

    it "rejects unrecognized options" $ do
      fromLeft (parseOptions [] Nothing [] ["--foo"]) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--foo'\nTry `my-spec --help' for more information.\n")

    it "sets configColorMode to ColorAuto" $ do
      configColorMode <$> parseOptions [] Nothing [] [] `shouldBe` Right ColorAuto

    context "when the same option is specified multiple times" $ do
      it "gives later occurrences precedence" $ do
        configColorMode <$> parseOptions [] Nothing [] ["--color", "--no-color"] `shouldBe` Right ColorNever

    context "with --help" $ do
      let Left (code, help) = Options.parseOptions defaultConfig "spec" [] Nothing [] ["--help"]

      it "returns ExitSuccess" $ do
        code `shouldBe` ExitSuccess

      it "prints help" $ do
        expected <- readFile "help.txt"
        help `shouldBe` expected

    context "with --color" $ do
      it "sets configColorMode to ColorAlways" $ do
        configColorMode <$> parseOptions [] Nothing [] ["--color"] `shouldBe` Right ColorAlways

    context "with --no-color" $ do
      it "sets configColorMode to ColorNever" $ do
        configColorMode <$> parseOptions [] Nothing [] ["--no-color"] `shouldBe` Right ColorNever

    context "with --diff" $ do
      it "sets configDiff to True" $ do
        configDiff <$> parseOptions [] Nothing [] ["--diff"] `shouldBe` Right True

    context "with --no-diff" $ do
      it "sets configDiff to False" $ do
        configDiff <$> parseOptions [] Nothing [] ["--no-diff"] `shouldBe` Right False

    context "with --print-slow-items" $ do
      it "sets configPrintSlowItems to N" $ do
        configPrintSlowItems <$> parseOptions [] Nothing [] ["--print-slow-items=5"] `shouldBe` Right (Just 5)

      it "defaults N to 10" $ do
        configPrintSlowItems <$> parseOptions [] Nothing [] ["--print-slow-items"] `shouldBe` Right (Just 10)

      it "rejects invalid values" $ do
        let msg = "my-spec: invalid argument `foo' for `--print-slow-items'\nTry `my-spec --help' for more information.\n"
        void (parseOptions [] Nothing [] ["--print-slow-items=foo"]) `shouldBe` Left (ExitFailure 1, msg)

      context "when N is 0" $ do
        it "disables the option" $ do
          configPrintSlowItems <$> parseOptions [] Nothing [] ["-p0"] `shouldBe` Right Nothing

    context "with --qc-max-success" $ do
      it "sets QuickCheck maxSuccess" $ do
        maxSuccess . configQuickCheckArgs <$> (parseOptions [] Nothing [] ["--qc-max-success", "23"]) `shouldBe`  Right 23

      context "when given an invalid argument" $ do
        it "returns an error message" $ do
          fromLeft (parseOptions [] Nothing [] ["--qc-max-success", "foo"]) `shouldBe` (ExitFailure 1, "my-spec: invalid argument `foo' for `--qc-max-success'\nTry `my-spec --help' for more information.\n")

    context "with --qc-max-shrinks" $ do
      it "sets QuickCheck maxShrinks" $ do
        maxShrinks . configQuickCheckArgs <$> (parseOptions [] Nothing [] ["--qc-max-shrinks", "23"]) `shouldBe`  Right 23

    context "with --depth" $ do
      it "sets depth parameter for SmallCheck" $ do
        configSmallCheckDepth <$> parseOptions [] Nothing [] ["--depth", "23"] `shouldBe` Right 23

    context "with --jobs" $ do
      it "sets number of concurrent jobs" $ do
        configConcurrentJobs <$> parseOptions [] Nothing [] ["--jobs=23"] `shouldBe` Right (Just 23)

      it "rejects values < 1" $ do
        let msg = "my-spec: invalid argument `0' for `--jobs'\nTry `my-spec --help' for more information.\n"
        void (parseOptions [] Nothing [] ["--jobs=0"]) `shouldBe` Left (ExitFailure 1, msg)

    context "when given a config file" $ do
      it "uses options from config file" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] Nothing [] [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"])] Nothing [] ["--color"] `shouldBe` Right ColorAlways

      it "rejects --help" $ do
        fromLeft (parseOptions [("~/.hspec", ["--help"])] Nothing [] []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--help' in config file ~/.hspec\n")

      it "rejects unrecognized options" $ do
        fromLeft (parseOptions [("~/.hspec", ["--invalid"])] Nothing [] []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--invalid' in config file ~/.hspec\n")

      it "rejects ambiguous options" $ do
        fromLeft (parseOptions [("~/.hspec", ["--fail"])] Nothing [] []) `shouldBe` (ExitFailure 1,
          unlines [
            "my-spec: option `--fail' is ambiguous; could be one of:"
          , "    --fail-on-focused      fail on focused spec items"
          , "    --fail-fast            abort on first failure"
          , "    --failure-report=FILE  read/write a failure report for use with --rerun"
          , "in config file ~/.hspec"
          ]
          )

      context "when the same option is specified multiple times" $ do
        it "gives later occurrences precedence" $ do
          configColorMode <$> parseOptions [("~/.hspec", ["--color", "--no-color"])] Nothing [] [] `shouldBe` Right ColorNever

    context "when given multiple config files" $ do
      it "gives later config files precedence" $ do
        configColorMode <$> parseOptions [("~/.hspec", ["--no-color"]), (".hspec", ["--color"])] Nothing [] [] `shouldBe` Right ColorAlways

    context "when given HSPEC_OPTIONS (deprecated)" $ do
      it "uses options from HSPEC_OPTIONS" $ do
        configColorMode <$> parseOptions [] (Just ["--no-color"]) [] [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [] (Just ["--no-color"]) [] ["--color"] `shouldBe` Right ColorAlways

      it "rejects unrecognized options" $ do
        fromLeft (parseOptions [] (Just ["--invalid"]) [] []) `shouldBe` (ExitFailure 1, "my-spec: unrecognized option `--invalid' from environment variable HSPEC_OPTIONS\n")

    context "when given an option as an environment variable" $ do
      it "sets config value from environment variable" $ do
        configColorMode <$> parseOptions [] Nothing [("HSPEC_COLOR", "no")] [] `shouldBe` Right ColorNever

      it "gives command-line options precedence" $ do
        configColorMode <$> parseOptions [] Nothing [("HSPEC_COLOR", "no")] ["--color"] `shouldBe` Right ColorAlways

      it "warns on unrecognized option values" $ do
        fmap configColorMode <$> Options.parseOptions defaultConfig "my-spec" [] Nothing [("HSPEC_COLOR", "foo")] [] `shouldBe` Right (["invalid value `foo' for environment variable HSPEC_COLOR"], ColorAuto)

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
