module ExamplesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Examples" $ do
    -- | Landing Page Examples
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

    -- | Quickcheck Examples
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)
