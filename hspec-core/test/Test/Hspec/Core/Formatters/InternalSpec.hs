module Test.Hspec.Core.Formatters.InternalSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Internal

spec :: Spec
spec = do
  describe "overwriteWith" $ do
    context "when old is null" $ do
      it "returns new" $ do
        ("" `overwriteWith` "foo") `shouldBe` "foo"

    context "when old and new have the same length" $ do
      it "overwrites old" $ do
        ("foo" `overwriteWith` "bar") `shouldBe` "\rbar"

    context "when old is shorter than new" $ do
      it "overwrites old" $ do
        ("ba" `overwriteWith` "foo") `shouldBe` "\rfoo"

    context "when old is longer than new" $ do
      it "overwrites old" $ do
        ("foobar" `overwriteWith` "foo") `shouldBe` "\rfoo   "

  describe "splitLines" $ do
    it "splits a string into chunks" $ do
      splitLines "foo\nbar\nbaz" `shouldBe` ["foo", "\n", "bar", "\n", "baz"]

    it "splits *arbitrary* strings into chunks" $ do
      property $ \ xs -> do
        mconcat (splitLines xs) `shouldBe` xs

    it "puts newlines into separate chunks" $ do
      property $ \ xs -> do
        filter (notElem '\n') (splitLines xs) `shouldBe` filter (not . null) (lines xs)
