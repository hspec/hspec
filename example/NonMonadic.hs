module Spec (main, spec) where

import Test.Hspec
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import Test.HUnit

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected

main :: IO ()
main = hspecX spec

spec :: Specs
spec = [
    describe "reverse" [
      it "reverses a list" $ do
        reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    , it "gives the original list, if applied twice" $ property $
        \(xs) -> reverse (reverse xs) == (xs :: [Int])
    ]
  ]
