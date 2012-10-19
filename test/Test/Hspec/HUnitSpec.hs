module Test.Hspec.HUnitSpec (main, spec) where

import           Test.Hspec.Meta
import           Util (capture__)
import           Control.Applicative

import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import           Test.Hspec.Internal (SpecTree(..))
import           Test.Hspec.HUnit
import           Test.HUnit

main :: IO ()
main = hspec spec

-- SpecTree does not have an Eq nor a Show instance, hence we map it to `Tree`.
data Tree = Group String [Tree] | Example String
  deriving (Eq, Show)

shouldYield :: Test -> [Tree] -> Expectation
a `shouldYield` b = (convert . fromHUnitTest) a `shouldBe` b
  where
    convert :: [SpecTree] -> [Tree]
    convert = map go
      where
        go :: SpecTree -> Tree
        go x = case x of
          SpecGroup s xs  -> Group s (map go xs)
          SpecExample s _ -> Example s

spec :: Spec
spec = do
  describe "fromHUnitTest" $ do
    let e = TestCase $ pure ()

    it "works for a TestCase" $ do
      e `shouldYield` [Example "<unlabeled>"]

    it "works for a labeled TestCase" $ do
      TestLabel "foo" e
        `shouldYield` [Example "foo"]

    it "works for a TestCase with nested labels" $ do
      (TestLabel "foo" . TestLabel "bar") e
        `shouldYield` [Group "foo" [Example "bar"]]

    it "works for a flat TestList" $ do
      TestList [e, e, e]
        `shouldYield` [Example "<unlabeled>", Example "<unlabeled>", Example "<unlabeled>"]

    it "works for a nested TestList" $ do
      (TestLabel "foo" . TestLabel "bar" . TestList) [TestLabel "one" e, TestLabel "two" e, TestLabel "three" e]
        `shouldYield` [Group "foo" [Group "bar" [Example "one", Example "two", Example "three"]]]

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
      r <- capture__ . runSpec $ do
        H.describe "foo" $ do
          H.it "bar" (assertFailure assertionText)
      r `shouldSatisfy` any (== assertionText)

    it "will show the failed assertion expected and actual values if available (e.g. assertEqual)" $ do
      r <- capture__ . runSpec $ do
        H.describe "foo" $ do
          H.it "bar" (assertEqual "trivial" (1::Int) 2)
      assertBool "should find assertion text" $ any (=="trivial") r
      assertBool "should find 'expected: 1'"  $ any (=="expected: 1") r
      assertBool "should find ' but got: 2'"  $ any (==" but got: 2") r
  where
    runSpec = H.hspecWith H.defaultConfig
