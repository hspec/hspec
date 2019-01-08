module Test.Hspec.Core.GoldenSpec (spec) where

import           Helper

import           Control.Monad (void)

import           Test.Hspec.Core.Golden
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H

import           System.Directory

{-# ANN module "HLint: ignore Reduce duplication" #-}

fixtureContent, fixtureTestName, fixtureUpdatedContent :: String
fixtureUpdatedContent = "different text"
fixtureContent = "simple text"
fixtureTestName = "id"

goldenTestDir, goldenFile, actualFile :: FilePath
goldenTestDir = ".hspec/id"
goldenFile = goldenTestDir ++ "/" ++ "golden"
actualFile = goldenTestDir ++ "/" ++ "actual"

fixtureTest :: String -> H.Spec
fixtureTest content =
  H.describe "id" $
    H.it "should work" $
      defaultGolden fixtureTestName content

removeFixtures :: IO ()
removeFixtures = removeDirectoryRecursive goldenTestDir

runSpec :: H.Spec -> IO [String]
runSpec = captureLines . H.hspecResult

spec :: Spec
spec =
  describe "Golden" $ after_ removeFixtures $ do
    context "when the test is executed for the first time" $ do
      it "should create a `golden` file" $ do
         void $ runSpec $ fixtureTest fixtureContent
         goldenFileContent <- readFile goldenFile
         goldenFileContent `shouldBe` fixtureContent

      it "shouldn't create a `actual` file" $ do
        void $ runSpec $ fixtureTest fixtureContent
        doesFileExist actualFile `shouldReturn` False

    context "when the output is updated" $
      context "when the test is executed a second time" $ do
        it "should create the `actual` output file" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureUpdatedContent
           actualFileContent <- readFile actualFile
           actualFileContent `shouldBe` fixtureUpdatedContent

        it "shouldn't overide the `golden` file" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureUpdatedContent
           goldenFileContent <- readFile goldenFile
           goldenFileContent `shouldBe` fixtureContent


    context "when the output is not updated" $
      context "when the test is executed a second time" $ do
        it "shouldn't change the `golden` file content" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureContent
           goldenFileContent <- readFile goldenFile
           goldenFileContent `shouldBe` fixtureContent

        it "shouldn't create the `actual` output file" $ do
           void $ runSpec $ fixtureTest fixtureContent
           void $ runSpec $ fixtureTest fixtureContent
           doesFileExist actualFile `shouldReturn` False
