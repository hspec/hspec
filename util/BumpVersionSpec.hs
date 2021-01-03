module BumpVersionSpec (spec) where

import           Test.Hspec

import           BumpVersion

spec :: Spec
spec = do
  describe "bumpVersion" $ do
    it "bumps version" $ do
      bumpVersion "&version 2.7.2\n" `shouldBe` "&version 2.7.3\n"
