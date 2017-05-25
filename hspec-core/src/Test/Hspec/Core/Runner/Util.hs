{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Runner.Util where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Data.List
import           Data.Char

import           Test.Hspec.Core.Example

extractLocation :: SomeException -> Maybe Location
extractLocation e = case fromException e of
#if MIN_VERSION_base(4,9,0)
  Just (ErrorCallWithLocation _ loc) -> parseCallStack loc
#else
  Just (ErrorCall _) -> Nothing
#endif
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
  where
    breakColon = fmap (drop 1) . break (== ':')
