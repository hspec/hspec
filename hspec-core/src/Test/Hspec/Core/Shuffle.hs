{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Shuffle (
  shuffleForest
#ifdef TEST
, shuffle
, mkArray
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Tree

import           System.Random
import           Control.Monad.ST
import           Data.STRef
import           Data.Array.ST

shuffleForest :: STRef st StdGen -> [Tree c a] -> ST st [Tree c a]
shuffleForest ref xs = (shuffle ref xs >>= mapM (shuffleTree ref))

shuffleTree :: STRef st StdGen -> Tree c a -> ST st (Tree c a)
shuffleTree ref t = case t of
  Node d xs -> Node d <$> shuffleForest ref xs
  NodeWithCleanup loc c xs -> NodeWithCleanup loc c <$> shuffleForest ref xs
  Leaf {} -> return t

shuffle :: STRef st StdGen -> [a] -> ST st [a]
shuffle ref xs = do
  arr <- mkArray xs
  bounds@(_, n) <- getBounds arr
  forM (range bounds) $ \ i -> do
    j <- randomIndex (i, n)
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr j vi
    return vj
  where
    randomIndex bounds = do
      (a, gen) <- randomR bounds <$> readSTRef ref
      writeSTRef ref gen
      return a

mkArray :: [a] -> ST st (STArray st Int a)
mkArray xs = newListArray (1, length xs) xs
