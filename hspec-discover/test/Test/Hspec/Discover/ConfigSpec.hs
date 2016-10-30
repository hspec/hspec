module Test.Hspec.Discover.ConfigSpec (main, spec) where

import           Helper

import           Test.Hspec.Discover.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseConfig" $ do
    let parse = parseConfig "hspec-discover"

    it "recognizes --nested" $ do
      parse ["--nested"] `shouldBe` Right (defaultConfig {configNested = True})

    it "recognizes --formatter" $ do
      parse ["--formatter", "someFormatter"] `shouldBe` Right (defaultConfig {configFormatter = Just "someFormatter"})

    it "recognizes --no-main" $ do
      parse ["--no-main"] `shouldBe` Right (defaultConfig {configNoMain = True})

    it "returns error message on unrecognized option" $ do
      parse ["--foo"] `shouldBe` (Left . unlines) [
          "hspec-discover: unrecognized option `--foo'"
        , ""
        , "Usage: hspec-discover SRC CUR DST [--module-name=NAME]"
        ]

    it "returns error message on unexpected argument" $ do
      parse ["foo"]   `shouldBe` (Left . unlines) [
          "hspec-discover: unexpected argument `foo'"
        , ""
        , "Usage: hspec-discover SRC CUR DST [--module-name=NAME]"
        ]

    it "returns error message on --formatter=<fmt> with --no-main" $ do
      parse ["--no-main", "--formatter=foo"] `shouldBe` (Left . unlines) [
          "hspec-discover: option `--formatter=<fmt>' does not make sense with `--no-main'"
        , ""
        , "Usage: hspec-discover SRC CUR DST [--module-name=NAME]"
        ]

    context "when option is given multiple times" $ do
      it "gives the last occurrence precedence" $ do
        parse ["--formatter", "foo", "--formatter", "bar"] `shouldBe` Right (defaultConfig {configFormatter = Just "bar"})
