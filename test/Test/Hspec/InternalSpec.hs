module Test.Hspec.InternalSpec (main, spec) where

import           Test.Hspec.Meta

import           Test.Hspec.Internal (quantify)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "quantify" $ do
    it "returns an amount and a word given an amount and word" $ do
      quantify 1 "thing" `shouldBe` "1 thing"

    it "returns a singular word given the number 1" $ do
      quantify 1 "thing" `shouldBe` "1 thing"

    it "returns a plural word given a number greater than 1" $ do
      quantify 2 "thing" `shouldBe` "2 things"

    it "returns a plural word given the number 0" $ do
      quantify 0 "thing" `shouldBe` "0 things"
