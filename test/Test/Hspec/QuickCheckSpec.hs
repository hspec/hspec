module Test.Hspec.QuickCheckSpec (main, spec) where

import           Test.Hspec.Meta
import           Util

import qualified Test.Hspec.Core as H
import           Test.Hspec.QuickCheck ()
import           Test.QuickCheck
import           System.IO.Silently

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A QuickCheck property as an example" $ do
    it "is specified with the `property` function" $ do
      hspecSummary [H.it "foo" . property $ \ b -> b || True]
      `shouldReturn` H.Summary 1 0

    context "when failing" $ do
      it "shows what falsified it" $ do
        (r, s) <- capture $ hspecSummary [H.it "quickcheck" (property $ \ i -> i == (i + 1 :: Int))]
        s `shouldBe` H.Summary 1 1
        lines r `shouldSatisfy` any (== "0")
