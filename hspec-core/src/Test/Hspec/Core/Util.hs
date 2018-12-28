-- | Stability: unstable
module Test.Hspec.Core.Util (
-- * String functions
  pluralize
, strip
, lineBreaksAt

-- * Working with paths
, Path
, joinPath
, formatRequirement
, filterPredicate

-- * Working with exception
, safeTry
, formatException
) where

import           Data.List
import           Data.Char (isSpace)
import           GHC.IO.Exception
import           Control.Exception
import           Control.Concurrent.Async

import           Test.Hspec.Core.Compat (showType)

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
pluralize :: Int -> String -> String
pluralize 1 s = "1 " ++ s
pluralize n s = show n ++ " " ++ s ++ "s"

-- | Strip leading and trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- |
-- ensure that lines are not longer than given `n`, insert line breaks at word
-- boundaries
lineBreaksAt :: Int -> String -> [String]
lineBreaksAt n input = case words input of
  []   -> []
  x:xs -> go (x, xs)
  where
    go :: (String, [String]) -> [String]
    go c = case c of
      (s, [])   -> [s]
      (s, y:ys) -> let r = s ++ " " ++ y in
        if length r <= n
          then go (r, ys)
          else s : go (y, ys)

-- |
-- A `Path` describes the location of a spec item within a spec tree.
--
-- It consists of a list of group descriptions and a requirement description.
type Path = ([String], String)

-- |
-- Join a `Path` with slashes.  The result will have a leading and a trailing
-- slash.
joinPath :: Path -> String
joinPath (groups, requirement) = "/" ++ intercalate "/" (groups ++ [requirement]) ++ "/"

-- |
-- Try to create a proper English sentence from a path by applying some
-- heuristics.
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
-- "ArithException (divide by zero)"
--
-- For `IOException`s the `IOErrorType` is included, as well.
formatException :: SomeException -> String
formatException err@(SomeException e) = case fromException err of
  Just ioe -> showType ioe ++ " of type " ++ showIOErrorType ioe ++ "\n" ++ show ioe
  Nothing  -> showType e ++ "\n" ++ show e
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
safeTry :: IO a -> IO (Either SomeException a)
safeTry action = withAsync (action >>= evaluate) waitCatch
