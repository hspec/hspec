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
