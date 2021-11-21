{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Hspec.Core.Formatters.Diff (
  Diff (..)
, diff
#ifdef TEST
, partition
, breakList
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (First)

import           Data.Char
import qualified Data.Algorithm.Diff as Diff

data Diff = First String | Second String | Both String
  deriving (Eq, Show)

diff :: String -> String -> [Diff]
diff expected actual = map (toDiff . fmap concat) $ Diff.getGroupedDiff (partition expected) (partition actual)

toDiff :: Diff.Diff String -> Diff
toDiff d = case d of
  Diff.First xs -> First xs
  Diff.Second xs -> Second xs
  Diff.Both xs _ -> Both xs

partition :: String -> [String]
partition = filter (not . null) . mergeBackslashes . breakList isAlphaNum
  where
    mergeBackslashes :: [String] -> [String]
    mergeBackslashes xs = case xs of
      ['\\'] : (splitEscape -> Just (escape, ys)) : zs -> ("\\" ++ escape) : ys : mergeBackslashes zs
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

splitEscape :: String -> Maybe (String, String)
splitEscape xs = splitNumericEscape xs <|> (msum $ map split escapes)
  where
    split :: String -> Maybe (String, String)
    split escape = (,) escape <$> stripPrefix escape xs

splitNumericEscape :: String -> Maybe (String, String)
splitNumericEscape xs = case span isDigit xs of
  ("", _) -> Nothing
  r -> Just r

escapes :: [String]
escapes = [
    "ACK"
  , "CAN"
  , "DC1"
  , "DC2"
  , "DC3"
  , "DC4"
  , "DEL"
  , "DLE"
  , "ENQ"
  , "EOT"
  , "ESC"
  , "ETB"
  , "ETX"
  , "NAK"
  , "NUL"
  , "SOH"
  , "STX"
  , "SUB"
  , "SYN"
  , "EM"
  , "FS"
  , "GS"
  , "RS"
  , "SI"
  , "SO"
  , "US"
  , "a"
  , "b"
  , "f"
  , "n"
  , "r"
  , "t"
  , "v"
  , "&"
  , "'"
  , "\""
  , "\\"
  ]
