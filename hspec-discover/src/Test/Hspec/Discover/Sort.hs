module Test.Hspec.Discover.Sort (
  NaturalSortChunk(..)
, naturalSortKey
) where

import           Data.Char

data NaturalSortChunk
  = NumericChunk { chunkNum :: Integer, chunkStr :: String }
  | StringChunk { chunkStr :: String }
  deriving (Eq, Show)

instance Ord NaturalSortChunk where
  compare (NumericChunk n1 s1) (NumericChunk n2 s2) =
    compare n1 n2 ~> compareLength s1 s2 ~> compare s1 s2
  compare (StringChunk s1) (StringChunk s2) = compare s1 s2
  compare (NumericChunk _ _) (StringChunk _) = LT
  compare (StringChunk _) (NumericChunk _ _) = GT

(~>) :: Ordering -> Ordering -> Ordering
EQ ~> a = a
a ~> _ = a
infixr 5 ~>

compareLength :: [a] -> [b] -> Ordering
compareLength [] [] = EQ
compareLength [] _ = LT
compareLength _ [] = GT
compareLength (_:xs) (_:ys) = compareLength xs ys

naturalSortKey :: String -> [NaturalSortChunk]
naturalSortKey s@(c:_)
  | isDigit c = NumericChunk (read num) num : naturalSortKey afterNum
  | otherwise = StringChunk             str : naturalSortKey afterStr
  where
    (num, afterNum) = span  isDigit s
    (str, afterStr) = break isDigit  s
naturalSortKey _ = []
