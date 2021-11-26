module Test.Hspec.Core.Formatters.Pretty.Unicode (
  ushow
, ushows
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Char

ushow :: String -> String
ushow xs = ushows xs ""

ushows :: String -> ShowS
ushows = uShowString

uShowString :: String -> ShowS
uShowString cs = showChar '"' . showLitString cs . showChar '"'

showLitString :: String -> ShowS
showLitString []         s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c   : cs) s = uShowLitChar c (showLitString cs s)

uShowLitChar :: Char -> ShowS
uShowLitChar c
  | isPrint c && not (isAscii c) = showChar c
  | otherwise = showLitChar c
