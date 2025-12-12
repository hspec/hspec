{-# LANGUAGE DeriveDataTypeable #-}
module Test.Hspec.Core.CompatSpec (spec) where

import           Prelude ()
import           Helper

import           System.Environment

data SomeType = SomeType

spec :: Spec
spec = do
  describe "showType" $ do
    it "shows unqualified name of type" $ do
      showType SomeType `shouldBe` "SomeType"

  describe "lookupEnv" $ do
    it "returns value of specified environment variable" $ do
      setEnv "FOO" "bar"
      lookupEnv "FOO" `shouldReturn` Just "bar"

    it "returns Nothing if specified environment variable is not set" $ do
      unsetEnv "FOO"
      lookupEnv "FOO" `shouldReturn` Nothing
