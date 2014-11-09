{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Contrib.HUnitSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Applicative

import           Test.Hspec.Core.Spec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

main :: IO ()
main = hspec spec

shouldYield :: Test -> [Tree (ActionWith ()) String] -> Expectation
a `shouldYield` b = map (Blind . fmap itemRequirement) <$> runSpecM (fromHUnitTest a) `shouldReturn` map Blind b

instance Eq a => Eq (Tree c a) where
  x == y = case (x, y) of
    (Node s1 xs, Node s2 ys) -> s1 == s2 && xs == ys
    (Leaf x1, Leaf x2) -> x1 == x2
    _ -> False

spec :: Spec
spec = do
  describe "fromHUnitTest" $ do
    let e = TestCase $ pure ()

    it "works for a TestCase" $ do
      e `shouldYield` [Leaf "<unlabeled>"]

    it "works for a labeled TestCase" $ do
      TestLabel "foo" e
        `shouldYield` [Leaf "foo"]

    it "works for a TestCase with nested labels" $ do
      (TestLabel "foo" . TestLabel "bar") e
        `shouldYield` [Node "foo" [Leaf "bar"]]

    it "works for a flat TestList" $ do
      TestList [e, e, e]
        `shouldYield` [Leaf "<unlabeled>", Leaf "<unlabeled>", Leaf "<unlabeled>"]

    it "works for a nested TestList" $ do
      (TestLabel "foo" . TestLabel "bar" . TestList) [TestLabel "one" e, TestLabel "two" e, TestLabel "three" e]
        `shouldYield` [Node "foo" [Node "bar" [Leaf "one", Leaf "two", Leaf "three"]]]
