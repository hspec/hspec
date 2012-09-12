module Test.Hspec.HUnitSpec (main, spec) where

import           Test.Hspec.Meta
import           Util

import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H
import           Test.Hspec.HUnit ()
import           Test.HUnit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "for Assertion" $ do
      it "returns Success if no assertion fails" $ do
        H.evaluateExample H.defaultConfig (assertEqual "" (23 :: Int) 23) `shouldReturn` H.Success

      it "returns Fail on assertFailure" $ do
        H.evaluateExample H.defaultConfig (assertFailure "foobar") `shouldReturn` H.Fail "foobar"

      it "returns Fail on failing assertEqual" $ do
        H.evaluateExample H.defaultConfig (assertEqual "" (42 :: Int) 23) `shouldReturn` H.Fail "expected: 42\n but got: 23"

      it "propagates exceptions" $ do
        H.evaluateExample H.defaultConfig (error "foobar" :: Assertion) `shouldThrow` errorCall "foobar"

  describe "HUnit TestCase as an example" $ do
    it "is specified with the HUnit `TestCase` data constructor" $ TestCase $ do
      hspecSummary [
          H.it "some behavior" (TestCase $ "foo" @?= "bar")
        , H.it "some behavior" (TestCase $ "foo" @?= "foo")
        ]
      `shouldReturn` H.Summary 2 1

    it "is the assumed example for IO() actions" $
      hspecSummary [
          H.it "some behavior" ("foo" @?= "bar")
        , H.it "some behavior" ("foo" @?= "foo")
        ]
      `shouldReturn` H.Summary 2 1

    it "will show the failed assertion text if available (e.g. assertBool)" $ do
      let assertionText = "some assertion text"
      r <- runSpec [H.describe "foo" [H.it "bar" (assertFailure assertionText)]]
      r `shouldSatisfy` any (== assertionText)

    it "will show the failed assertion expected and actual values if available (e.g. assertEqual)" $ do
      r <- runSpec [H.describe "foo" [H.it "bar" (assertEqual "trivial" (1::Int) 2)]]
      assertBool "should find assertion text" $ any (=="trivial") r
      assertBool "should find 'expected: 1'"  $ any (=="expected: 1") r
      assertBool "should find ' but got: 2'"  $ any (==" but got: 2") r
