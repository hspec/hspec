module Test.Hspec.QuickCheckSpec (main, spec) where

import           Helper

import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.QuickCheck as H
import           Test.Hspec.Options (defaultOptions)
import qualified Test.Hspec.Config as Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prop" $ do
    it "is a shortcut to use properties as examples" $ do
      silence . H.hspecResult $ do
        H.describe "read" $ do
          H.prop "is inverse to show" $ \x -> (read . show) x == (x :: Int)
      `shouldReturn` H.Summary 1 0

  describe "command-line option qc-max-size" $ do
    let testWithMaxSize s = do
          c <- Config.getConfig defaultOptions "" ["--qc-max-size", s]
          silence . H.hspecWith c $ do
            H.describe "" $ do
              H.prop "list has at most 1 element" $ \l -> length (l :: [Int]) <= 1
    it "generates lists with at most one element" $ do
      testWithMaxSize "2" `shouldReturn` H.Summary 1 0
    it "generates lists with more than one element" $ do
      testWithMaxSize "3" `shouldReturn` H.Summary 1 1
