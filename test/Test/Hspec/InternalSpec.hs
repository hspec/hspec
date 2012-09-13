module Test.Hspec.InternalSpec (main, spec) where

import           Test.Hspec.Meta

import           System.IO.Silently
import qualified Test.Hspec.Internal as H
import qualified Test.Hspec.Config as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "evaluateExample" $ do

    context "for Bool" $ do
      it "returns Success on True" $ do
        H.evaluateExample H.defaultConfig True `shouldReturn` H.Success

      it "returns Fail on False" $ do
        H.evaluateExample H.defaultConfig False `shouldReturn` H.Fail ""

      it "propagates exceptions" $ do
        H.evaluateExample H.defaultConfig (error "foobar" :: Bool) `shouldThrow` errorCall "foobar"

    context "for Expectation" $ do
      it "returns Success if all expectations hold" $ do
        H.evaluateExample H.defaultConfig (23 `shouldBe` (23 :: Int)) `shouldReturn` H.Success

      it "returns Fail if an expectation does not hold" $ do
        H.evaluateExample H.defaultConfig (23 `shouldBe` (42 :: Int)) `shouldReturn` H.Fail "expected: 42\n but got: 23"

      it "propagates exceptions" $ do
        H.evaluateExample H.defaultConfig (error "foobar" :: Expectation) `shouldThrow` errorCall "foobar"

      it "silences any output to stdout" $ do
        (capture . H.evaluateExample H.defaultConfig $ putStrLn "foobar") `shouldReturn` ("", H.Success)
