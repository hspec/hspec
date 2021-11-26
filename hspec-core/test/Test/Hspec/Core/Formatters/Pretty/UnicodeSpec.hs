module Test.Hspec.Core.Formatters.Pretty.UnicodeSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.Unicode

spec :: Spec
spec = do
  describe "ushow" $ do
    it "retains unicode characters" $ do
      ushow "foo-\955-bar" `shouldBe` "\"foo-\955-bar\""

    it "is inverted by read" $ do
      property $ \ xs ->
        read (ushow xs) `shouldBe` xs
