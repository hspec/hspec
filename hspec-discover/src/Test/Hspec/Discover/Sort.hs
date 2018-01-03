module Test.Hspec.Discover.Sort (
  sortNatural
, NaturalSortKey
, naturalSortKey
) where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Ord

sortNatural :: [String] -> [String]
sortNatural = sortBy (comparing naturalSortKey)

data NaturalSortKey = NaturalSortKey [Chunk] String
  deriving (Eq, Ord)

data Chunk = Numeric Integer Int | Textual String
  deriving (Eq, Ord)

naturalSortKey :: String -> NaturalSortKey
naturalSortKey = uncurry NaturalSortKey . (chunks &&& id)
  where
    chunks [] = []
    chunks s@(c:_)
      | isDigit c = Numeric (read num) (length num) : chunks afterNum
      | otherwise = Textual (map toLower str) : chunks afterStr
      where
        (num, afterNum) = span  isDigit s
        (str, afterStr) = break isDigit s
