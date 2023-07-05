module BumpVersionSpec (spec) where

import           Test.Hspec

import           BumpVersion

spec :: Spec
spec = do
  describe "bumpVersion" $ do
    it "bumps minor version" $ do
      bumpVersion Minor "version: &version 2.7.2\n" `shouldBe` "version: &version 2.7.3\n"

    it "bumps major version" $ do
      bumpVersion Major "version: &version 2.7.2\n" `shouldBe` "version: &version 2.8.0\n"
