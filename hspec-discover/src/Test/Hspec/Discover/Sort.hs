module Test.Hspec.Discover.Sort (
  NaturalSortChunk(..)
, naturalSortKey
) where

import           Control.Arrow
import           Data.Char

data NaturalSortChunk
  = NumericChunk { chunkNum :: Integer, chunkStrLen :: Int }
  | StringChunk { chunkLCStr :: String }
  deriving (Eq, Ord, Show)

naturalSortKey :: String -> ([NaturalSortChunk], String)
naturalSortKey = chunks &&& id where
  chunks s@(c:_)
    | isDigit c = NumericChunk (read num) (length num) : chunks afterNum
    | otherwise = StringChunk (map toLower str) : chunks afterStr
    where
      (num, afterNum) = span  isDigit s
      (str, afterStr) = break isDigit s
  chunks _ = []
