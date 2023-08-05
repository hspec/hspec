{-# LANGUAGE  ViewPatterns #-}
-- | Stability: unstable
module Test.Hspec.Core.Util (
-- * String functions
  pluralize
, strip
, lineBreaksAt
, stripAnsi

-- * Working with paths
, Path
, joinPath
, formatRequirement
, filterPredicate

-- * Working with exceptions
, safeTry
, formatException
, formatExceptionWith
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (join)

import           Data.Char (isSpace)
import           GHC.IO.Exception
import           Control.Concurrent.Async

-- |
-- @pluralize count singular@ pluralizes the given @singular@ word unless given
-- @count@ is 1.
--
-- Examples:
--
-- >>> pluralize 0 "example"
-- "0 examples"
--
-- >>> pluralize 1 "example"
-- "1 example"
--
-- >>> pluralize 2 "example"
-- "2 examples"
--
-- @since 2.0.0
pluralize :: Int -> String -> String
pluralize 1 s = "1 " ++ s
pluralize n s = show n ++ " " ++ s ++ "s"

-- | Strip leading and trailing whitespace
--
-- @since 2.0.0
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- |
-- Ensure that lines are not longer than given `n`, insert line breaks at word
-- boundaries
--
-- @since 2.0.0
lineBreaksAt :: Int -> String -> [String]
lineBreaksAt n = concatMap f . lines
  where
    f input = case words input of
      []   -> []
      x:xs -> go (x, xs)

    go :: (String, [String]) -> [String]
    go c = case c of
      (s, [])   -> [s]
      (s, y:ys) -> let r = s ++ " " ++ y in
        if length r <= n
          then go (r, ys)
          else s : go (y, ys)

-- |
-- Remove ANSI color escape sequences.
--
-- @since 2.11.0
stripAnsi :: String -> String
stripAnsi = go
  where
    go input = case input of
      '\ESC' : '[' : (dropWhile (`elem` "0123456789;") -> 'm' : xs) -> go xs
      x : xs -> x : go xs
      [] -> []

-- |
-- A `Path` describes the location of a spec item within a spec tree.
--
-- It consists of a list of group descriptions and a requirement description.
--
-- @since 2.0.0
type Path = ([String], String)

-- |
-- Join a `Path` with slashes.  The result will have a leading and a trailing
-- slash.
--
-- @since 2.5.4
joinPath :: Path -> String
joinPath (groups, requirement) = "/" ++ intercalate "/" (groups ++ [requirement]) ++ "/"

-- |
-- Try to create a proper English sentence from a path by applying some
-- heuristics.
--
-- @since 2.0.0
formatRequirement :: Path -> String
formatRequirement (groups, requirement) = groups_ ++ requirement
  where
    groups_ = case break (any isSpace) groups of
      ([], ys) -> join ys
      (xs, ys) -> join (intercalate "." xs : ys)

    join xs = case xs of
      [x] -> x ++ " "
      ys  -> concatMap (++ ", ") ys

-- | A predicate that can be used to filter a spec tree.
--
-- @since 2.0.0
filterPredicate :: String -> Path -> Bool
filterPredicate pattern path =
     pattern `isInfixOf` plain
  || pattern `isInfixOf` formatted
  where
    plain = joinPath path
    formatted = formatRequirement path

-- | The function `formatException` converts an exception to a string.
--
-- This is different from `show`.  The type of the exception is included, e.g.:
--
-- >>> formatException (toException DivideByZero)
-- "ArithException\ndivide by zero"
--
-- For `IOException`s the `IOErrorType` is included, as well.
--
-- @since 2.0.0
formatException :: SomeException -> String
formatException = formatExceptionWith show

-- | @since 2.11.5
formatExceptionWith :: (SomeException -> String) -> SomeException -> String
formatExceptionWith showException err@(SomeException e) = case fromException err of
  Just ioe -> showType ioe ++ " of type " ++ showIOErrorType ioe ++ "\n" ++ showException (toException ioe)
  Nothing  -> showType e ++ "\n" ++ showException (SomeException e)
  where
    showIOErrorType :: IOException -> String
    showIOErrorType ioe = case ioe_type ioe of
      AlreadyExists -> "AlreadyExists"
      NoSuchThing -> "NoSuchThing"
      ResourceBusy -> "ResourceBusy"
      ResourceExhausted -> "ResourceExhausted"
      EOF -> "EOF"
      IllegalOperation -> "IllegalOperation"
      PermissionDenied -> "PermissionDenied"
      UserError -> "UserError"
      UnsatisfiedConstraints -> "UnsatisfiedConstraints"
      SystemError -> "SystemError"
      ProtocolError -> "ProtocolError"
      OtherError -> "OtherError"
      InvalidArgument -> "InvalidArgument"
      InappropriateType -> "InappropriateType"
      HardwareFault -> "HardwareFault"
      UnsupportedOperation -> "UnsupportedOperation"
      TimeExpired -> "TimeExpired"
      ResourceVanished -> "ResourceVanished"
      Interrupted -> "Interrupted"

-- | @safeTry@ evaluates given action and returns its result.  If an exception
-- occurs, the exception is returned instead.  Unlike `try` it is agnostic to
-- asynchronous exceptions.
--
-- @since 2.0.0
safeTry :: IO a -> IO (Either SomeException a)
safeTry action = withAsync (action >>= evaluate) waitCatch
