module Test.Hspec.OptionsSpec (main, spec) where

import           Helper

import           Test.Hspec.Options hiding (parseOptions)
import qualified Test.Hspec.Options as Options

main :: IO ()
main = hspec spec

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
