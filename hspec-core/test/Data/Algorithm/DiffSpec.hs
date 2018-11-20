module Data.Algorithm.DiffSpec (spec) where

import           Data.Algorithm.Diff
import           Helper hiding (First)

spec :: Spec
spec = do
  describe "getDiff" $ do
    it "both are empty" $ do
      getDiff "" "" `shouldBe` []
    it "first is empty" $ do
      getDiff "str" "" `shouldBe` [First 's', First 't', First 'r']
    it "second is empty" $ do
      getDiff "" "str" `shouldBe` [Second 's', Second 't', Second 'r']

    it "equal" $ do
      getDiff "str" "str" `shouldBe` [Both 's' 's', Both 't' 't', Both 'r' 'r']
    it "different element" $ do
      getDiff "str" "sur" `shouldBe` [Both 's' 's', First 't', Second 'u', Both 'r' 'r']

    it "mixed elements" $ do
      getDiff [1, 2, 1, 2, 1, 2] [1, 2, 3, 1, 2, 3 :: Int]
        `shouldBe` [Both 1 1, Both 2 2, First 1, First 2, Second 3, Both 1 1, Both 2 2, Second 3]
    it "no common elements" $ do
      getDiff [0 .. 4] [5 .. 9 :: Int]
        `shouldBe` [ First 0, First 1, First 2, First 3, First 4
                   , Second 5, Second 6, Second 7, Second 8, Second 9]

    it "first is longer" $ do
      getDiff "x" "zxcvb" `shouldBe` [Second 'z', Both 'x' 'x', Second 'c', Second 'v', Second 'b']
    it "second is longer" $ do
      getDiff "zxcvb" "x" `shouldBe` [First 'z', Both 'x' 'x', First 'c', First 'v', First 'b']
