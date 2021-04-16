module Test.Hspec.Core.ShuffleSpec (spec) where

import           Prelude ()
import           Helper

import qualified Test.Hspec.Core.Shuffle as H
import           Test.Hspec.Core.Tree

import           Data.Array.ST
import           Control.Monad.ST
import           Data.STRef
import           System.Random

spec :: Spec
spec = do
  describe "shuffleForest" $ do
    let
      shuffleForest :: Int -> [Tree () Int] -> [Tree () Int]
      shuffleForest seed xs = runST $ do
        gen <- newSTRef (mkStdGen seed)
        H.shuffleForest gen xs

    it "shuffles a forest" $ do
      shuffleForest 2
        [Leaf 1, Leaf 2, Leaf 3] `shouldBe`
        [Leaf 3, Leaf 1, Leaf 2]

    it "recurses into Node" $ do
      shuffleForest 1
        [Node "foo" [Node "bar" [Leaf 1, Leaf 2, Leaf 3]]] `shouldBe`
        [Node "foo" [Node "bar" [Leaf 2, Leaf 3, Leaf 1]]]

    it "recurses into NodeWithCleanup" $ do
      shuffleForest 1
        [NodeWithCleanup Nothing () [NodeWithCleanup Nothing () [Leaf 1, Leaf 2, Leaf 3]]] `shouldBe`
        [NodeWithCleanup Nothing () [NodeWithCleanup Nothing () [Leaf 2, Leaf 3, Leaf 1]]]

  describe "shuffle" $ do
    it "shuffles a list" $ do
      runST $ do
        gen <- newSTRef (mkStdGen 2)
        H.shuffle gen [1, 2, 3 :: Int]
      `shouldBe` [3, 1, 2]

  describe "mkArray" $ do
    it "creates an STArray from a list" $ do
      runST (H.mkArray [1, 2, 3 :: Int] >>= getElems) `shouldBe` [1, 2, 3]
