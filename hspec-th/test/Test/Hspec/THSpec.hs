{-# LANGUAGE CPP, TemplateHaskell #-}
module Test.Hspec.THSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Core.Spec

import qualified Test.Hspec.TH as H

main :: IO ()
main = hspec spec

infix 1 `shouldHaveLocation`

shouldHaveLocation :: Spec -> Location -> Expectation
x `shouldHaveLocation` loc = do
  [Leaf item] <- runSpecM x
  itemLocation item `shouldBe` Just loc

spec :: Spec
spec = do
  describe "it" $ do
    it "adds source locations to spec item" $ do
      $(H.it) "foo" True `shouldHaveLocation` Location __FILE__ __LINE__ 9 ExactLocation
