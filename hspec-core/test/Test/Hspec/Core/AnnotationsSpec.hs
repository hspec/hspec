module Test.Hspec.Core.AnnotationsSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Annotations

newtype A = A Int
  deriving (Eq, Show)

newtype B = B Int
  deriving (Eq, Show)

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
      it "gives precedence to the value that was added last" $ do
        let annotations = setValue (A 42) $  setValue (A 23) mempty
        getValue annotations `shouldBe` Just (A 42)
