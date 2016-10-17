{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Formatters.Diff (
  Diff (..)
, diff
#ifndef TEST
, breakList
#endif
) where

import           Data.Char
import           Data.Algorithm.Diff

breakList :: (a -> Bool) -> [a] -> [[a]]
breakList _ [] = []
breakList p xs = case break p xs of
  (y, ys) -> y `cons` case span p ys of
    (z, zs) -> z `cons` breakList p zs
  where
    cons x
      | null x = id
      | otherwise = (x :)

diff :: String -> String -> [Diff String]
diff expected actual = map (fmap concat) $ getGroupedDiff (breakList isAlphaNum expected) (breakList isAlphaNum actual)
