{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Runner.Util (
  extractLocation

-- for testing
, parseCallStack
, parseLocation
, parseSourceSpan
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Data.List
import           Data.Char
import           Data.Maybe

import           Test.Hspec.Core.Example

extractLocation :: SomeException -> Maybe Location
extractLocation e = locationFromErrorCall e <|> locationFromPatternMatchFail e

locationFromErrorCall :: SomeException -> Maybe Location
locationFromErrorCall e = case fromException e of
#if MIN_VERSION_base(4,9,0)
  Just (ErrorCallWithLocation _ loc) -> parseCallStack loc
#else
  Just (ErrorCall _) -> Nothing
#endif
  Nothing -> Nothing

locationFromPatternMatchFail :: SomeException -> Maybe Location
locationFromPatternMatchFail e = case fromException e of
  Just (PatternMatchFail s) -> listToMaybe (words s) >>= parseSourceSpan
  Nothing -> Nothing

parseCallStack :: String -> Maybe Location
parseCallStack input = case reverse (lines input) of
  [] -> Nothing
  line : _ -> findLocation line
  where
    findLocation xs = case xs of
      [] -> Nothing
      _ : ys -> case stripPrefix prefix xs of
        Just zs -> parseLocation (takeWhile (not . isSpace) zs)
        Nothing -> findLocation ys
    prefix = ", called at "

parseLocation :: String -> Maybe Location
parseLocation input = case fmap breakColon (breakColon input) of
  (file, (line, column)) -> Location file <$> readMaybe line <*> readMaybe column

parseSourceSpan :: String -> Maybe Location
parseSourceSpan input = case breakColon input of
  (file, xs) -> (uncurry $ Location file) <$> (tuple <|> colonSeparated)
    where
      lineAndColumn :: String
      lineAndColumn = takeWhile (/= '-') xs

      tuple :: Maybe (Int, Int)
      tuple = readMaybe lineAndColumn

      colonSeparated :: Maybe (Int, Int)
      colonSeparated = case breakColon lineAndColumn of
        (l, c) -> (,) <$> readMaybe l <*> readMaybe c

breakColon :: String -> (String, String)
breakColon = fmap (drop 1) . break (== ':')
