-- |
-- /NOTE:/ This module is not meant for public consumption.  For user
-- documentation look at http://hspec.github.io/hspec-discover.html.
module Test.Hspec.Discover.Sort (
  sortNaturallyBy
, NaturalSortKey
, naturalSortKey
) where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Ord

sortNaturallyBy :: (a -> (String, Int)) -> [a] -> [a]
sortNaturallyBy f = sortBy (comparing ((\ (k, t) -> (naturalSortKey k, t)) . f))

data NaturalSortKey = NaturalSortKey [Chunk]
  deriving (Eq, Ord)

data Chunk = Numeric Integer Int | Textual [(Char, Char)]
  deriving (Eq, Ord)

naturalSortKey :: String -> NaturalSortKey
naturalSortKey = NaturalSortKey . chunks
  where
    chunks [] = []
    chunks s@(c:_)
      | isDigit c = Numeric (read num) (length num) : chunks afterNum
      | otherwise = Textual (map (toLower &&& id) str) : chunks afterStr
      where
        (num, afterNum) = span  isDigit s
        (str, afterStr) = break isDigit s
