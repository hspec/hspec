module Test.Hspec.Discover.SortSpec (main, spec) where

import           Helper

import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Test.Hspec.Discover.Sort
import           Test.QuickCheck

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "naturalSortKey" $ do
    it "is injective" $ property $ \ a  b -> do
      a /= b ==> naturalSortKey a /= naturalSortKey b

    context "compares" $ do
      it "empty first" $ property $ \a -> not (null a) ==> do
        "" <=> a `shouldBe` LT
        a <=> "" `shouldBe` GT

      it "numbers first" $ do
        "Hello2World" <=> "Hello World" `shouldBe` LT
        "Hello World" <=> "Hello2World" `shouldBe` GT

      it "string segments" $ do
        "Hello2World9" <=> "Hello2World!0" `shouldBe` LT
        "Hello2World!0" <=> "Hello2World9" `shouldBe` GT

      it "numeric segments" $ do
        "3.1.415" <=> "3.14.15" `shouldBe` LT
        "3.14.15" <=> "3.1.415" `shouldBe` GT

      it "numeric ties by string length" $ do
        "Hello 02 World" <=> "Hello 002 World" `shouldBe` LT
        "Hello 002 World" <=> "Hello 02 World" `shouldBe` GT

      it "case squashing" $ do
        "Helloworld1" <=> "HelloWorld2" `shouldBe` LT
        "HelloWorld2" <=> "helloworld1" `shouldBe` GT

      it "string ties by case" $ do
        "HelloWorld3" <=> "Helloworld3" `shouldBe` LT
        "Helloworld3" <=> "HelloWorld3" `shouldBe` GT

    context "sorts" $ do
      it "for humans" $ do
        sortBy (<=>)
          [ "z" ++ show (n :: Int) ++ ".txt"
          | n <- [1, 10] ++ [100..102] ++ [11..19] ++ [2] ++ [20] ++ [3..9]
          ] `shouldBe`
          [ "z" ++ show (n :: Int) ++ ".txt"
          | n <- [1..20] ++ [100..102]
          ]

  where
    (<=>) = comparing naturalSortKey
    infix 5 <=>
