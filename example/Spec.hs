module Main (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  -- | Specify a concrete example.
  --
  -- Describe a "reverse" function, providing two examples
  -- to test its functionality:
  
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])

  -- | Specify a pending example.
  --
  -- If you want to textually specify a behavior but do not have an example yet,
  -- use this:

  describe "fancyFormatter" $ do
    it "can format text in a way that everyone likes" $
      pending

  -- | Specify a pending example with a reason for why it's pending.

  describe "fancyFormatter" $ do
    it "can format text in a way that everyone likes" $
      pendingWith "waiting for clarification from the designers"
