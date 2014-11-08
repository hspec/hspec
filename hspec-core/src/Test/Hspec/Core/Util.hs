module Test.Hspec.Core.Util (
  pluralize
, formatException
, lineBreaksAt
, safeTry
, Path
, filterPredicate
, formatRequirement
, strip
) where

import           Data.List
import           Data.Char (isSpace)
import           GHC.IO.Exception
import qualified Control.Exception as E
import           Control.Concurrent.Async

import           Test.Hspec.Compat (showType)

pluralize :: Int -> String -> String
pluralize 1 s = "1 " ++ s
pluralize n s = show n ++ " " ++ s ++ "s"

-- | Convert an exception to a string.
--
-- This is different from `show`.  The type of the exception is included, e.g.
-- `E.toException E.DivideByZero` is turned into:
--
-- @
-- "ArithException (divide by zero)"
-- @
--
-- For `IOException`s the `IOErrorType` is included.
formatException :: E.SomeException -> String
formatException err@(E.SomeException e) = case E.fromException err of
  Just ioe -> showType ioe ++ " of type " ++ showIOErrorType ioe ++ " (" ++ show ioe ++ ")"
  Nothing  -> showType e ++ " (" ++ show e ++ ")"
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

safeTry :: IO a -> IO (Either E.SomeException a)
safeTry action = withAsync (action >>= E.evaluate) waitCatch

-- |
-- A tuple that represents the location of an example within a spec.
--
-- It consists of a list of group descriptions and a requirement description.
type Path = ([String], String)

-- | A predicate that can be used to filter specs.
filterPredicate :: String -> Path -> Bool
filterPredicate pattern path@(groups, requirement) =
     pattern `isInfixOf` plain
  || pattern `isInfixOf` formatted
  where
    plain = intercalate "/" (groups ++ [requirement])
    formatted = formatRequirement path

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

-- ensure that lines are not longer then given `n`, insert line breaks at word
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

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
