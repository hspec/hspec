module Test.Hspec.Core.ShuffleSpec (spec) where

import           Prelude ()
import           Helper

import qualified Test.Hspec.Core.Shuffle as H

import           Data.Array.ST
import           Control.Monad.ST
import           Data.STRef
import           System.Random

spec :: Spec
spec = do
  describe "shuffle" $ do
    it "shuffles a list" $ do
      runST $ do
        gen <- newSTRef (mkStdGen 2)
        H.shuffle gen [1, 2, 3 :: Int]
      `shouldBe` [3, 1, 2]

  describe "mkArray" $ do
    it "creates an STArray from a list" $ do
      runST (H.mkArray [1, 2, 3 :: Int] >>= getElems) `shouldBe` [1, 2, 3]
