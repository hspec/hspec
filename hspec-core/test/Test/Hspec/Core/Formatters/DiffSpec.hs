module Test.Hspec.Core.Formatters.DiffSpec (spec) where

import           Helper
import           Data.Char

import           Test.Hspec.Core.Formatters.Diff

spec :: Spec
spec = do
  describe "breakList" $ do
    context "with a list where the predicate matches at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "foo bar  baz" `shouldBe` ["foo", " ", "bar", "  ", "baz"]

    context "with a list where the predicate does not match at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "  foo bar  baz  " `shouldBe` ["  ", "foo", " ", "bar", "  ", "baz", "  "]
