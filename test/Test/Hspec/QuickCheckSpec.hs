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
    let testWithOptions opts = do
          c <- Config.getConfig defaultOptions "" opts
          silence . H.hspecWith c $ do
            H.describe "path1" $ do
              H.prop "path2" $ \l -> length (l :: [Int]) <= 1 -- every list has at most one element
    it "generates lists with at most one element" $ do
      testWithOptions ["--qc-max-size", "2"] `shouldReturn` H.Summary 1 0
    it "generates lists with more than one element" $ do
      testWithOptions ["--qc-max-size", "3"] `shouldReturn` H.Summary 1 1

    -- This one is a bit tricky.  We need a function that fails when given a
    -- maxSize, but succeeds when no maxSize is present.  We use the following
    -- trick:  The rerun feature makes use of the fact that paths to tests are
    -- unique. We therefore give it two different tests with identical paths.
    -- The first test just always fails, which ensures that path1/path2 is
    -- tested again on a rerun.
    let failWithOptions opts = do
          c <- Config.getConfig defaultOptions "" opts
          silence . H.hspecWith c $ do
            H.describe "path1" $ do
              H.prop "path2" $ False
    it "uses qc-max-size from failure report on rerun" $ do
      failWithOptions ["--qc-max-size", "2"] `shouldReturn` H.Summary 1 1
      -- when maxSize is 2, then every list has at most one element, so the test succeeds
      testWithOptions ["--rerun"] `shouldReturn` H.Summary 1 0
    it "doesn't use qc-max-size from failure report when rerun option isn't given" $ do
      failWithOptions ["--qc-max-size", "2"] `shouldReturn` H.Summary 1 1
      -- maxSize isn't set, so there are lists with more than one element, so the test fails
      testWithOptions [] `shouldReturn` H.Summary 1 1

  describe "command-line option qc-max-discard" $ do
    let testWithOptions opts = do
          c <- Config.getConfig defaultOptions "" opts
          silence . H.hspecWith c $ do
            H.describe "path1" $ do
              H.prop "path2" $ \l -> null (l :: [Int]) ==> True -- discards all except the empty list
    it "fails when we require no discards" $ do
      testWithOptions ["--qc-max-discard", "0"] `shouldReturn` H.Summary 1 1
    it "succeeds when we allow lots of discards" $ do
      testWithOptions ["--qc-max-discard", "100"] `shouldReturn` H.Summary 1 0
