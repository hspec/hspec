{-# LANGUAGE DeriveDataTypeable #-}
module Test.Hspec.CompatSpec (main, spec) where

import           Helper

import           Test.Hspec.Compat
import           Data.Typeable

data SomeType = SomeType
  deriving Typeable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showType" $ do
    it "shows unqualified name of type" $ do
      showType SomeType `shouldBe` "SomeType"

  describe "showFullType (currently unused)" $ do
    it "shows fully qualified name of type" $ do
      showFullType SomeType `shouldBe` "Test.Hspec.CompatSpec.SomeType"
