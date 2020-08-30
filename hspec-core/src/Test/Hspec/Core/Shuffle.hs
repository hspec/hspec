{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Shuffle (
  shuffle
#ifdef TEST
, mkArray
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           System.Random
import           Control.Monad.ST
import           Data.STRef
import           Data.Array.ST

shuffle :: STRef s StdGen -> [a] -> ST s [a]
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

mkArray :: [a] -> ST s (STArray s Int a)
mkArray xs = newListArray (1, length xs) xs
