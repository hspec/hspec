module Main (main, spec) where

import Test.Hspec.Core
import Test.Hspec.Expectations
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Specs
spec = [
    describe "reverse" [
      it "reverses a list" $ do
        reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    , it "gives the original list, if applied twice" $ property $
        \(xs) -> reverse (reverse xs) == (xs :: [Int])
    ]
  ]
