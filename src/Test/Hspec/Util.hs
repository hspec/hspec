module Test.Hspec.Util (
  quantify
, formatRequirement
, safeEvaluate
, filterPredicate
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

-- | A predicate that can be used to filter a specs.
filterPredicate :: String -> [String] -> String -> Bool
filterPredicate pattern descriptions requirement =
     pattern `isInfixOf` path
  || pattern `isInfixOf` formatted
  where
    path = intercalate "/" (descriptions ++ [requirement])
    formatted = formatRequirement descriptions requirement

-- |
-- Format a list of nested descriptions and a requirement by applying some
-- heuristics.
formatRequirement :: [String] -> String -> String
formatRequirement groups requirement = groups_ ++ requirement
  where
    groups_ = case break (any isSpace) groups of
      ([], ys) -> join ys
      (xs, ys) -> join (intercalate "." xs : ys)

    join xs = case xs of
      [x] -> x ++ " "
      ys  -> concatMap (++ ", ") ys
