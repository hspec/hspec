{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Contrib.HUnitSpec (spec) where

import           Helper

import           Test.Hspec.Core.Spec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

shouldYield :: Test -> [Tree () String] -> Expectation
a `shouldYield` b = bimapForest (const ()) itemRequirement . snd <$> runSpecM (fromHUnitTest a) `shouldReturn` b

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
