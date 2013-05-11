module Test.Hspec.HUnitSpec (main, spec) where

import           Helper

import           Test.Hspec.Core.Type (SpecTree(..), Item(..), runSpecM)
import           Test.Hspec.HUnit
import           Test.HUnit

main :: IO ()
main = hspec spec

-- SpecTree does not have an Eq nor a Show instance, hence we map it to `Tree`.
data Tree = Group String [Tree] | Example String
  deriving (Eq, Show)

shouldYield :: Test -> [Tree] -> Expectation
a `shouldYield` b = (convert . runSpecM . fromHUnitTest) a `shouldBe` b
  where
    convert :: [SpecTree] -> [Tree]
    convert = map go
      where
        go :: SpecTree -> Tree
        go x = case x of
          SpecGroup s xs  -> Group s (map go xs)
          SpecItem item -> Example (itemRequirement item)

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
