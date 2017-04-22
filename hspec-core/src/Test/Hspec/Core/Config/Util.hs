module Test.Hspec.Core.Config.Util where

import           System.Console.GetOpt

import           Test.Hspec.Core.Util

modifyHelp :: (String -> String) -> OptDescr a -> OptDescr a
modifyHelp modify (Option s n a help) = Option s n a (modify help)

mkUsageInfo :: String -> [OptDescr a] -> String
mkUsageInfo title = usageInfo title . addLineBreaksForHelp . condenseNoOptions

addLineBreaksForHelp :: [OptDescr a] -> [OptDescr a]
addLineBreaksForHelp options = map (modifyHelp addLineBreaks) options
  where
    withoutHelpWidth = maxLength . usageInfo "" . map removeHelp
    helpWidth = 80 - withoutHelpWidth options

    addLineBreaks = unlines . lineBreaksAt helpWidth

    maxLength = maximum . map length . lines
    removeHelp = modifyHelp (const "")

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
