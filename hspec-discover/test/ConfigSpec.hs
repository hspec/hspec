module ConfigSpec (main, spec) where

import           Test.Hspec.Meta

import           Config

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

    it "returns error message on unrecognized option" $ do
      parse ["--foo"] `shouldBe` (Left . unlines) [
          "hspec-discover: unrecognized option `--foo'"
        , ""
        , "Usage: hspec-discover SRC CUR DST [--formatter=FORMATTER]"
        ]

    it "returns error message on unexpected argument" $ do
      parse ["foo"]   `shouldBe` (Left . unlines) [
          "hspec-discover: unexpected argument `foo'"
        , ""
        , "Usage: hspec-discover SRC CUR DST [--formatter=FORMATTER]"
        ]

    context "when option is given multiple times" $ do
      it "gives the last occurrence precedence" $ do
        parse ["--formatter", "foo", "--formatter", "bar"] `shouldBe` Right (defaultConfig {configFormatter = Just "bar"})
