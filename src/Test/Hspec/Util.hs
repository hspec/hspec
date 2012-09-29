module Test.Hspec.Util (
  quantify
, safeEvaluate
, Path
, filterPredicate
, formatRequirement
) where

import           Data.List
import           Data.Char (isSpace)
import           Control.Applicative
import qualified Control.Exception as E

-- | Create a more readable display of a quantity of something.
--
-- Examples:
--
-- >>> quantify 0 "example"
-- "0 examples"
--
-- >>> quantify 1 "example"
-- "1 example"
--
-- >>> quantify 2 "example"
-- "2 examples"
quantify :: Int -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"

safeEvaluate :: IO a -> IO (Either E.SomeException a)
safeEvaluate action = (Right <$> action) `E.catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT
  -- (ctrl-c).  All AsyncExceptions are re-thrown (not just UserInterrupt)
  -- because all of them indicate severe conditions and should not occur during
  -- normal operation.
    E.Handler $ \e -> E.throw (e :: E.AsyncException)

  , E.Handler $ \e -> (return . Left) (e :: E.SomeException)
  ]

-- |
-- A tuple that describes the location of a test within an example in a Spec.
--
-- It consists of a list of group descriptions and a requirement description,
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
