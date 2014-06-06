module Test.Hspec.Util (
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
import qualified Control.Exception as E
import           Control.Exception.Enclosed (tryAny)

import           Test.Hspec.Compat (showType)

-- | Create a more readable display of a quantity of something.
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

-- | Convert an exception to a string.
--
-- The type of the exception is included.  Here is an example:
--
-- >>> import Control.Applicative
-- >>> import Control.Exception
-- >>> either formatException show <$> (try . evaluate) (1 `div` 0)
-- "ArithException (divide by zero)"
formatException :: E.SomeException -> String
formatException (E.SomeException e) = showType e ++ " (" ++ show e ++ ")"

safeTry :: IO a -> IO (Either E.SomeException a)
safeTry = tryAny

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
