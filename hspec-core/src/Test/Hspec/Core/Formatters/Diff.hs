{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Hspec.Core.Formatters.Diff (
  Diff (..)
, diff

, LineDiff(..)
, lineDiff

, splitLines

#ifdef TEST
, partition
, breakList
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Char
import qualified Data.Algorithm.Diff as Diff

data Diff = First String | Second String | Both String
  deriving (Eq, Show)

data LineDiff =
    LinesFirst [String]
  | LinesSecond [String]
  | LinesBoth [String]
  | SingleLineDiff [Diff]
  | LinesOmitted Int
  deriving (Eq, Show)

lineDiff :: Maybe Int -> String -> String -> [LineDiff]
lineDiff context expected actual = maybe id applyContext context $ singleLineDiffs diffs
  where
    diffs :: [LineDiff]
    diffs = Diff.getGroupedDiff expectedLines actualLines <&> \ case
      Diff.First xs -> LinesFirst xs
      Diff.Second xs -> LinesSecond xs
      Diff.Both xs _ -> LinesBoth xs

    expectedLines :: [String]
    expectedLines = splitLines expected

    actualLines :: [String]
    actualLines = splitLines actual

singleLineDiffs :: [LineDiff] -> [LineDiff]
singleLineDiffs = go
  where
    go = \ case
      [] -> []
      LinesFirst [first_] : LinesSecond [second_] : xs -> SingleLineDiff (diff first_ second_) : go xs
      x : xs -> x : go xs

splitLines :: String -> [String]
splitLines = go
  where
    go :: String -> [String]
    go xs = case break (== '\n') xs of
      (ys, '\n' : zs) -> ys : go zs
      _ -> [xs]

data TrimMode = FirstChunk | Chunk | LastChunk

applyContext :: Int -> [LineDiff] -> [LineDiff]
applyContext context = \ diffs -> case diffs of
  [] -> []
  x : xs -> trimChunk FirstChunk x (go xs)
  where
    omitThreshold :: Int
    omitThreshold = 3

    go :: [LineDiff] -> [LineDiff]
    go diffs = case diffs of
      [] -> []
      [x] -> trimChunk LastChunk x []
      x : xs -> trimChunk Chunk x (go xs)

    trimChunk :: TrimMode -> LineDiff -> [LineDiff] -> [LineDiff]
    trimChunk mode chunk = case chunk of
      LinesBoth allLines | meetsThreshold -> keep start . (LinesOmitted omitted :) . keep end
        where
          meetsThreshold :: Bool
          meetsThreshold = omitThreshold <= omitted

          lastLineHasNL = case (mode, reverse allLines) of
            (LastChunk, "" : _) -> True
            _ -> False

          omitted :: Int
          omitted = n - keepStart - keepEnd

          keepStart :: Int
          keepStart = case mode of
            FirstChunk -> 0
            _ -> context

          keepEnd :: Int
          keepEnd = case mode of
            LastChunk -> 0
            _ -> context

          n :: Int
          n = length allLines - case lastLineHasNL of
            False -> 0
            True -> 1

          start :: [String]
          start = take keepStart allLines

          end :: [String]
          end = drop (keepStart + omitted) allLines

      _ -> (chunk :)

keep :: [String] -> [LineDiff] -> [LineDiff]
keep xs
  | null xs = id
  | otherwise = (LinesBoth xs :)

diff :: String -> String -> [Diff]
diff expected actual = case commonPrefixAndSuffix expectedChunks actualChunks of
  -- Avoid the general diff algorithm when the only change is an insertion or
  -- deletion between common boundaries.  This case can contain many chunks.
  (prefix, [], insertion, suffix) -> chunk Both prefix ++ chunk Second insertion ++ chunk Both suffix
  (prefix, deletion, [], suffix) -> chunk Both prefix ++ chunk First deletion ++ chunk Both suffix
  (prefix, xs, ys, suffix) -> chunk Both prefix ++ diffs xs ys ++ chunk Both suffix
  where
    diffs :: [String] -> [String] -> [Diff]
    diffs xs ys = map (toDiff . fmap concat) $ Diff.getGroupedDiff xs ys

    expectedChunks :: [String]
    expectedChunks = partition expected

    actualChunks :: [String]
    actualChunks = partition actual

    chunk :: (String -> Diff) -> [String] -> [Diff]
    chunk constructor = \ case
      [] -> []
      xs -> [constructor (concat xs)]

commonPrefixAndSuffix :: Eq a => [a] -> [a] -> ([a], [a], [a], [a])
commonPrefixAndSuffix expected actual = (prefix, expectedMiddle, actualMiddle, suffix)
  where
    (prefix, expectedSuffix, actualSuffix) = commonPrefix expected actual
    (suffix, expectedMiddle, actualMiddle) = commonSuffix expectedSuffix actualSuffix

    commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
    commonPrefix = go []
      where
        go acc (x : xs) (y : ys) | x == y = go (x : acc) xs ys
        go acc xs ys = (reverse acc, xs, ys)

    commonSuffix :: Eq a => [a] -> [a] -> ([a], [a], [a])
    commonSuffix xs ys = case commonPrefix (reverse xs) (reverse ys) of
      (s, e, a) -> (reverse s, reverse e, reverse a)

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
