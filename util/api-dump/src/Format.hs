{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Format (
  format
#ifdef TEST
, skipParens
, breakOutsideParens
, joinDefinitions
, formatRecord
, formatSum
#endif
) where

import Data.Char
import Data.List
import Data.Bifunctor

format :: String -> String
format = unlines . map (formatSum . formatRecord) . joinDefinitions . lines

joinDefinitions :: [String] -> [String]
joinDefinitions = go
  where
    go = \ case
      c : (span startsWithSpace -> (cs, ds)) | isClass c -> joinClassDefinition (c : cs) : go ds
      x : (' ' : y) : ys -> go $ (x ++ ' ' : dropWhile isSpace y) : ys
      d : ds -> d : go ds
      [] -> []
    isClass = isPrefixOf "class"
    startsWithSpace = isPrefixOf " "

joinClassDefinition :: [String] -> String
joinClassDefinition = intercalate "\n" . go
  where
    go = \ case
      c : (' ' : ' ' : ' ' : ' ' : ' ' : ' ' : d) : cs -> go $ (c ++ ' ' : dropWhile isSpace d) : cs
      c : cs -> c : go cs
      [] -> []

formatSum :: String -> String
formatSum input
  | '|' `notElem` input = input
  | otherwise = sum_
  where
    sum_ :: String
    sum_ = case break (== '=') input of
      (xs, '=' : ys) -> intercalate "\n" $ strip xs : constructors ys
      _ -> input

    constructors :: String -> [String]
    constructors xs = case break (== '|') xs of
      (ys, '|' : zs) -> ("  | " ++ strip ys) : constructors zs
      _ -> ["  | " ++ strip xs]

formatRecord :: String -> String
formatRecord = record
  where
    record :: String -> String
    record input = case break (== '{') input of
      (_,  '{' : '-' : _) -> input
      (xs, '{' : ys) -> intercalate "\n" $ strip xs : fields ys
      _ -> input

    fields :: String -> [String]
    fields input = case breakOutsideParens (== ',') input of
      (xs, _ : ys) -> ("  " ++ dropWhile isSpace xs) : fields ys
      (xs, "") -> ["  " ++ (dropWhile isSpace $ take (pred $ length xs) xs)]

breakOutsideParens :: (Char -> Bool) -> String -> (String, String)
breakOutsideParens p = go
  where
    go = \ case
      '(' : xs -> case skipParens xs of
        (ys, zs) -> first ('(' :) $ first (ys ++) (go zs)
      x : xs | p x -> ([], x : xs)
      x : xs ->  first (x :) (go xs)
      [] -> ([], [])

skipParens :: String -> (String, String)
skipParens xs = case break isParen xs of
  (ys, ')' : zs) -> (ys ++ [')'], zs)
  (ys, '(' : zs) -> case skipParens zs of
    (yys, bbb) -> case skipParens bbb of
      (yyys, zzs) -> (ys ++ '(' : yys ++ yyys, zzs)
  (ys, zs) -> (ys, zs)
  where
    isParen c = c == '(' || c == ')'

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
