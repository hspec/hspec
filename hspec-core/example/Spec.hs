module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [2, 1]

    it "gives the original list, if applied twice" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])

    it "gives the original list, if applied twice" $ property $
      pending

    it "reverses a list" $ do
      "foo\nbar\nbaz  \n" `shouldBe` "foo  \nbar\nbaz\n"
