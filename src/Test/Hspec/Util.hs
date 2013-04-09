module Test.Hspec.Util (
  pluralize
, lineBreaksAt
, safeTry
, Path
, filterPredicate
, formatRequirement
, readMaybe
, getEnv
, strip
, stdGenToInteger
, stdGenFromInteger
) where

import           Data.Int (Int32)
import           Data.List
import           Data.Maybe
import           Data.Char (isSpace)
import           Control.Applicative
import qualified Control.Exception as E
import qualified System.Environment as Environment
import           System.Random (StdGen)

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

safeTry :: IO a -> IO (Either E.SomeException a)
safeTry action = (Right <$> (action >>= E.evaluate)) `E.catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT
  -- (ctrl-c).  All AsyncExceptions are re-thrown (not just UserInterrupt)
  -- because all of them indicate severe conditions and should not occur during
  -- normal operation.
    E.Handler $ \e -> E.throwIO (e :: E.AsyncException)

  , E.Handler $ \e -> (return . Left) (e :: E.SomeException)
  ]

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

-- NOTE: base-4.6.0.0 provides a function with that name and type.  For
-- compatibility with earlier versions, we define our own version here.
readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

getEnv :: String -> IO (Maybe String)
getEnv key = either (const Nothing) Just <$> safeTry (Environment.getEnv key)

-- ensure that lines are not longer then given `n`, insert line beraks at word
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

-- | Converts a 'StdGen' into an 'Integer'. Assumes
--   StdGens to be encoded as two positive 'Int32's and
--   $show (StdGen a b) = show a ++ " " ++ show b$.
stdGenToInteger :: StdGen -> Integer
stdGenToInteger stdGen =
  let [a, b] = map read . words $ show stdGen
  in b * fromIntegral (maxBound :: Int32) + a

-- | Inverse of 'stdGenToInteger'.
stdGenFromInteger :: Integer -> StdGen
stdGenFromInteger n =
  let (a, b) = quotRem n (fromIntegral (maxBound :: Int32))
  in read (show b ++ " " ++ show a)
