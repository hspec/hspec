{-# LANGUAGE TypeApplications #-}
module Test.Hspec.Discover.SortSpec (main, spec) where

import           Helper

import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Test.Hspec.Discover.Sort

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "naturalSortKey" $ do
    context "compares" $ do
      it "reflexively" $ do
        "" <=> "" `shouldBe` EQ
        "Hello2World" <=> "Hello2World" `shouldBe` EQ
        "123Hello456" <=> "123Hello456" `shouldBe` EQ
        "" <=> "" `shouldBe` EQ
      it "empty first" $ do
        "" <=> "Hello2World" `shouldBe` LT
        "Hello2World" <=> "" `shouldBe` GT
      it "numbers first" $ do
        "Hello2World" <=> "Hello World" `shouldBe` LT
        "Hello World" <=> "Hello2World" `shouldBe` GT
      it "string segments" $ do
        "Hello2World9" <=> "Hello2World!0" `shouldBe` LT
        "Hello2World!0" <=> "Hello2World9" `shouldBe` GT
      it "numeric segments" $ do
        "3.1.415" <=> "3.14.15" `shouldBe` LT
        "3.14.15" <=> "3.1.415" `shouldBe` GT
      it "numeric ties by string" $ do
        "Hello 02 World" <=> "Hello 002 World" `shouldBe` LT
        "Hello 002 World" <=> "Hello 02 World" `shouldBe` GT
    context "sorts" $ do
      it "for humans" $ do
        sortBy (<=>)
          [ "z" ++ show @Int n ++ ".txt"
          | n <- [1, 10] ++ [100..102] ++ [11..19] ++ [2] ++ [20] ++ [3..9]
          ] `shouldBe`
          [ "z" ++ show @Int n ++ ".txt"
          | n <- [1..20] ++ [100..102]
          ]
  where
    (<=>) = comparing naturalSortKey
    infix 4 <=>
