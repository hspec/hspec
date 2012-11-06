{-# LANGUAGE DeriveDataTypeable #-}
module Test.Hspec.CompatSpec (main, spec) where

import           Test.Hspec.Meta

import           Test.Hspec.Compat
import           Data.Typeable

data SomeType = SomeType
  deriving Typeable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showType" $ do
    it "shows fully qualified name of type" $ do
      showType SomeType `shouldBe` "Test.Hspec.CompatSpec.SomeType"
