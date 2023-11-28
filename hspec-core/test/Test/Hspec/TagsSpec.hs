{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.TagsSpec (spec) where

import           Prelude ()
import           Helper

import           Mock
import qualified Test.Hspec.Core.Spec as H

import qualified Data.Map as Map
import           Test.Hspec.Core.Config
import qualified Test.Hspec.Core.Config.Options as Options

import           Test.Hspec.Tags as Tags

spec :: Spec
spec = do
  let
    hspec args = withArgs args . hspecSilent

  context "with --tags" $ do
    let
      foo args = do
        e1 <- newMock
        e2 <- newMock
        e3 <- newMock
        e4 <- newMock
        hspec args $ do
          Tags.use
          Tags.exclude "slow"
          H.it "example 1" >>> tag "slow" $ mockAction e1
          H.it "example 2" >>> tag "bar" $ mockAction e2
          H.it "example 3" >>> tag "slow" >>> tag "bar" $ mockAction e3
          H.it "example 4" $ mockAction e4
        (,,,) <$> mockCounter e1 <*> mockCounter e2 <*> mockCounter e3 <*> mockCounter e4

    it "" $ do
      foo [] `shouldReturn` (0, 1, 0, 1)

    it "" $ do
      foo ["--tag=slow"] `shouldReturn` (1, 0, 1, 0)

    it "" $ do
      foo ["--tag=bar"] `shouldReturn` (0, 1, 0, 0)

    it "" $ do
      foo ["--tag=+slow"] `shouldReturn` (1, 1, 1, 1)

    context "with --tags" $ do
      let parseOptions args = snd <$> Options.parseOptions defaultConfig { configCustomOptions = [("hspec-tags", [tagsOption])] } "my-spec" [] Nothing [] args

      it "" $ do
        getTagFilters <$> parseOptions ["--tags", "foo"] `shouldBe` Right (Map.fromList [("foo", Select)])

      it "" $ do
        getTagFilters <$> parseOptions ["--tags", "foo", "--tags", "-foo"] `shouldBe` Right (Map.fromList [("foo", Discard)])

      it "" $ do
        getTagFilters <$> parseOptions ["--tags", "foo -foo"] `shouldBe` Right (Map.fromList [("foo", Discard)])

      it "" $ do
        getTagFilters <$> parseOptions ["--tags", "-foo +foo"] `shouldBe` Right (Map.fromList [])

      it "" $ do
        getTagFilters <$> parseOptions ["--tags", "foo +foo"] `shouldBe` Right (Map.fromList [])

  describe "parseTagFilters" $ do
    it "" $ do
      parseTagFilters "foo" `shouldBe` [("foo", Just Select)]

    it "" $ do
      parseTagFilters "-foo" `shouldBe` [("foo", Just Discard)]

    it "" $ do
      parseTagFilters "+foo" `shouldBe` [("foo", Nothing)]

    it "" $ do
      parseTagFilters "foo bar -baz" `shouldBe` [("foo", Just Select), ("bar", Just Select), ("baz", Just Discard)]
