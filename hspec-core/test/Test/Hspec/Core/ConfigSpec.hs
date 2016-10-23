module Test.Hspec.Core.ConfigSpec (spec) where

import           Helper
import           System.Directory
import           System.FilePath

import           Test.Hspec.Core.Config

spec :: Spec
spec = do
  describe "readConfigFiles" $ around_ (withEnvironment []) $ around_ inTempDirectory $ do
    it "reads .hspec" $ do
      dir <- getCurrentDirectory
      let name = dir </> ".hspec"
      writeFile name "--diff"
      readConfigFiles `shouldReturn` [(name, ["--diff"])]

    it "reads ~/.hspec" $ do
      let name = "my-home/.hspec"
      createDirectory "my-home"
      writeFile name "--diff"
      withEnvironment [("HOME", "my-home")] $ do
        readConfigFiles `shouldReturn` [(name, ["--diff"])]

    context "without $HOME" $ do
      it "returns empty list" $ do
        readConfigFiles `shouldReturn` []

    context "without current directory" $ do
      it "returns empty list" $ do
        dir <- getCurrentDirectory
        removeDirectory dir
        readConfigFiles `shouldReturn` []
