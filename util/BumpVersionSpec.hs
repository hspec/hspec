module BumpVersionSpec (spec) where

import           Test.Hspec

import           BumpVersion

spec :: Spec
spec = do
  describe "bumpVersion" $ do
    it "bumps minor version" $ do
      bumpVersion Minor "&version 2.7.2\n" `shouldBe` "&version 2.7.3\n"

    it "bumps major version" $ do
      bumpVersion Major "&version 2.7.2\n" `shouldBe` "&version 2.8.0\n"
