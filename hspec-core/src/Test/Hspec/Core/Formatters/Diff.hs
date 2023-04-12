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

data Diff = First String | Second String | Both String | Omitted Int
  deriving (Eq, Show)

-- |
-- Split a string at line boundaries.
--
-- >>> splitLines "foo\nbar\nbaz"
-- ["foo\n","bar\n","baz"]
--
-- prop> concat (splitLines xs) == xs
splitLines :: String -> [String]
splitLines = go
  where
    go xs = case break (== '\n') xs of
      (ys, '\n' : zs) -> (ys ++ ['\n']) : go zs
      ("", "") -> []
      _ -> [xs]

data TrimMode = FirstChunck | Chunck | LastChunck

trim :: Int -> [Diff] -> [Diff]
trim context = \ chunks -> case chunks of
  [] -> []
  x : xs -> trimChunk FirstChunck x (go xs)
  where
    omitThreshold = 3

    go chunks = case chunks of
      [] -> []
      [x] -> trimChunk LastChunck x []
      x : xs -> trimChunk Chunck x (go xs)

    trimChunk mode chunk = case chunk of
      Both xs | omitted >= omitThreshold -> keep start . (Omitted omitted :) . keep end
        where
          omitted :: Int
          omitted = n - keepStart - keepEnd

          keepStart :: Int
          keepStart = case mode of
            FirstChunck -> 0
            _ -> succ context

          keepEnd :: Int
          keepEnd = case mode of
            LastChunck -> 0
            _ -> if xs `endsWith` "\n" then context else succ context

          n :: Int
          n = length allLines

          allLines :: [String]
          allLines = splitLines xs

          start :: [String]
          start = take keepStart allLines

          end :: [String]
          end = drop (keepStart + omitted) allLines
      _ -> (chunk :)

    keep xs
      | null xs = id
      | otherwise = (Both (concat xs) :)

diff :: Maybe Int -> String -> String -> [Diff]
diff context expected actual = maybe id trim context $ map (toDiff . fmap concat) $ Diff.getGroupedDiff (partition expected) (partition actual)

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
splitEscape xs = splitNumericEscape xs <|> msum (map split escapes)
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
