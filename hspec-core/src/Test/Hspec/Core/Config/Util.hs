module Test.Hspec.Core.Config.Util where

import           System.Console.GetOpt

condenseNoOptions :: [OptDescr a] -> [OptDescr a]
condenseNoOptions options = case options of
  Option "" [optionA] arg help : Option "" [optionB] _ _ : ys | optionB == ("no-" ++ optionA) ->
    Option "" ["[no-]" ++ optionA] arg help : condenseNoOptions ys
  x : xs -> x : condenseNoOptions xs
  [] -> []

formatOrList :: [String] -> String
formatOrList xs = case xs of
  [] -> ""
  x : ys -> (case ys of
    [] -> x
    _ : [] -> x ++ " or "
    _ : _ : _ -> x ++ ", ") ++ formatOrList ys
