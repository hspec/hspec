module Test.Hspec.HUnitSpec (main, spec) where

import           Test.Hspec.ShouldBe

import qualified Test.Hspec as H
import           Test.Hspec.HUnit ()
import           Test.HUnit
import           System.IO.Silently
import           System.IO

main :: IO ()
main = hspecX spec

spec :: Spec
spec = do
  describe "HUnit TestCase as an example" $ do
    it "is specified with the HUnit `TestCase` data constructor" $
      TestCase $ assertBool "example" True

    it "is the assumed example for IO() actions" $
      assertBool "example" True

    it "will show the failed assertion text if available (e.g. assertBool)" $ do
      let assertionText = "some assertion text"
      r <- runSpec [H.describe "foo" [H.it "bar" (assertFailure assertionText)]]
      r `shouldSatisfy` any (== assertionText)

    it "will show the failed assertion expected and actual values if available (e.g. assertEqual)" $ do
      r <- runSpec [H.describe "foo" [H.it "bar" (assertEqual "trivial" (1::Int) 2)]]
      assertBool "should find assertion text" $ any (=="trivial") r
      assertBool "should find 'expected: 1'"  $ any (=="expected: 1") r
      assertBool "should find ' but got: 2'"  $ any (==" but got: 2") r

  where
    runSpec s = (lines . fst) `fmap` capture (H.hHspec stdout s)
