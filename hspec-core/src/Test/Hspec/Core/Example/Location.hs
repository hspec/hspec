{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Example.Location (
  Location(..)
, extractLocation

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
import           GHC.IO.Exception

-- | @Location@ is used to represent source locations.
data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show, Read)

extractLocation :: SomeException -> Maybe Location
extractLocation e =
      locationFromErrorCall e
  <|> locationFromPatternMatchFail e
  <|> locationFromRecConError e
  <|> locationFromIOException e

locationFromErrorCall :: SomeException -> Maybe Location
locationFromErrorCall e = case fromException e of
#if MIN_VERSION_base(4,9,0)
  Just (ErrorCallWithLocation err loc) ->
    parseCallStack loc <|>
#else
  Just (ErrorCall err) ->
#endif
    fromPatternMatchFailureInDoExpression err
  Nothing -> Nothing

locationFromPatternMatchFail :: SomeException -> Maybe Location
locationFromPatternMatchFail e = case fromException e of
  Just (PatternMatchFail s) -> listToMaybe (words s) >>= parseSourceSpan
  Nothing -> Nothing

locationFromRecConError :: SomeException -> Maybe Location
locationFromRecConError e = case fromException e of
  Just (RecConError s) -> listToMaybe (words s) >>= parseSourceSpan
  Nothing -> Nothing

locationFromIOException :: SomeException -> Maybe Location
locationFromIOException e = case fromException e of
  Just (IOError {ioe_type = UserError, ioe_description = xs}) -> fromPatternMatchFailureInDoExpression xs
  Just _ -> Nothing
  Nothing -> Nothing

fromPatternMatchFailureInDoExpression :: String -> Maybe Location
fromPatternMatchFailureInDoExpression input =
  stripPrefix "Pattern match failure in do expression at " input >>= parseSourceSpan

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
