{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Formatters.Diff (
  Diff (..)
, diff
#ifdef TEST
, partition
, breakList
#endif
) where

import           Data.Char
import           Data.Algorithm.Diff

diff :: String -> String -> [Diff String]
diff expected actual = map (fmap concat) $ getGroupedDiff (partition expected) (partition actual)

partition :: String -> [String]
partition = mergeBackslashes . breakList isAlphaNum
  where
    mergeBackslashes xs = case xs of
      ['\\'] : (y : ys) : zs -> ['\\', y] : ys : mergeBackslashes zs
      z : zs -> z : mergeBackslashes zs
      [] -> []

breakList :: (a -> Bool) -> [a] -> [[a]]
breakList _ [] = []
breakList p xs = case break p xs of
  (y, ys) -> map return y ++ case span p ys of
    (z, zs) -> z `cons` breakList p zs
  where
    cons x
      | null x = id
      | otherwise = (x :)
