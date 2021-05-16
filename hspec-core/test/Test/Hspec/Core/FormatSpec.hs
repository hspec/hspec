module Test.Hspec.Core.FormatSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Exception

import           Test.Hspec.Core.Format

spec :: Spec
spec = do
  describe "monadic" $ do
    context "on exception" $ do
      it "propagates" $ do
        format <- monadic id (\ _ -> throwIO DivideByZero)
        format (Done []) `shouldThrow` (== DivideByZero)

      it "does not hang" $ do
        format <- monadic id (\ _ -> throwIO DivideByZero)
        format (Done []) `shouldThrow` (== DivideByZero)
        format (Done [])
