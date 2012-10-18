module Test.Hspec.HUnitSpec (main, spec) where

import           Test.Hspec.Meta
import           Util (capture_)

import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import           Test.Hspec.HUnit ()
import           Test.HUnit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HUnit TestCase as an example" $ do
    it "is specified with the HUnit `TestCase` data constructor" $ TestCase $ do
      runSpec $ do
        H.it "some behavior" (TestCase $ "foo" @?= "bar")
        H.it "some behavior" (TestCase $ "foo" @?= "foo")
      `shouldReturn` H.Summary 2 1

    it "is the assumed example for IO() actions" $ do
      runSpec $ do
        H.it "some behavior" ("foo" @?= "bar")
        H.it "some behavior" ("foo" @?= "foo")
      `shouldReturn` H.Summary 2 1

    it "will show the failed assertion text if available (e.g. assertBool)" $ do
      let assertionText = "some assertion text"
      r <- capture_ . runSpec $ do
        H.describe "foo" $ do
          H.it "bar" (assertFailure assertionText)
      r `shouldSatisfy` any (== assertionText)

    it "will show the failed assertion expected and actual values if available (e.g. assertEqual)" $ do
      r <- capture_ . runSpec $ do
        H.describe "foo" $ do
          H.it "bar" (assertEqual "trivial" (1::Int) 2)
      assertBool "should find assertion text" $ any (=="trivial") r
      assertBool "should find 'expected: 1'"  $ any (=="expected: 1") r
      assertBool "should find ' but got: 2'"  $ any (==" but got: 2") r
  where
    runSpec = H.hspecWith H.defaultConfig
