{-# LANGUAGE DeriveDataTypeable #-}
module Test.Hspec.Core.AnnotationsSpec (spec) where

import           Prelude ()
import           Helper

import           Data.Typeable

import           Test.Hspec.Core.Annotations

newtype A = A Int
  deriving (Eq, Show, Typeable)

newtype B = B Int
  deriving (Eq, Show, Typeable)

spec :: Spec
spec = do
  describe "Annotations" $ do
    it "can store a value" $ do
      let annotations = setValue (A 23) mempty
      getValue annotations `shouldBe` Just (A 23)

    it "can store multiple values of different types" $ do
      let annotations = setValue (B 42) $ setValue (A 23) mempty
      getValue annotations `shouldBe` Just (A 23)
      getValue annotations `shouldBe` Just (B 42)

    context "when a value of the same type is added multiple times" $ do
      it "gives the value that was added last precedence" $ do
        let annotations = setValue (A 42) $  setValue (A 23) mempty
        getValue annotations `shouldBe` Just (A 42)
