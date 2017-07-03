{-# LANGUAGE DeriveDataTypeable #-}
module Test.Hspec.Core.CompatSpec (spec) where

import           Helper
import           System.SetEnv
import           Data.Typeable

data SomeType = SomeType
  deriving Typeable

spec :: Spec
spec = do
  describe "showType" $ do
    it "shows unqualified name of type" $ do
      showType SomeType `shouldBe` "SomeType"

  describe "showFullType (currently unused)" $ do
    it "shows fully qualified name of type" $ do
      showFullType SomeType `shouldBe` "Test.Hspec.Core.CompatSpec.SomeType"

  describe "lookupEnv" $ do
    it "returns value of specified environment variable" $ do
      setEnv "FOO" "bar"
      lookupEnv "FOO" `shouldReturn` Just "bar"

    it "returns Nothing if specified environment variable is not set" $ do
      unsetEnv "FOO"
      lookupEnv "FOO" `shouldReturn` Nothing
