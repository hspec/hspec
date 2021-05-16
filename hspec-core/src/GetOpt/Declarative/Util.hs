{-# LANGUAGE CPP #-}
module GetOpt.Declarative.Util (mkUsageInfo, mapOptDescr) where

import           Prelude ()
import           Test.Hspec.Core.Compat

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

mapOptDescr :: (a -> b) -> OptDescr a -> OptDescr b
#if MIN_VERSION_base(4,7,0)
mapOptDescr = fmap
#else
mapOptDescr f opt = case opt of
  Option short long arg help -> Option short long (mapArgDescr f arg) help

mapArgDescr :: (a -> b) -> ArgDescr a -> ArgDescr b
mapArgDescr f arg = case arg of
  NoArg a -> NoArg (f a)
  ReqArg parse name -> ReqArg (fmap f parse) name
  OptArg parse name -> OptArg (fmap f parse) name
#endif
