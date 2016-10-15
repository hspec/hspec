{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Discover {-# WARNING
  "This module is used by @hspec-discover@.  It is not part of the public API and may change at any time."
  #-} (
  Spec
, hspec
, IsFormatter (..)
, hspecWithFormatter
, postProcessSpec
, describe
) where

import           Prelude hiding (mapM)
import           Control.Applicative
import           Data.Maybe
import           Data.List
import           Data.Traversable
import           Control.Monad.Trans.State

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Formatters
import           Test.Hspec.Core.Util (safeTry)

class IsFormatter a where
  toFormatter :: a -> IO Formatter

instance IsFormatter (IO Formatter) where
  toFormatter = id

instance IsFormatter Formatter where
  toFormatter = return

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec = locationHeuristicFromFile

locationHeuristicFromFile :: FilePath -> Spec -> Spec
locationHeuristicFromFile file spec = do
  mInput <- either (const Nothing) Just <$> (runIO . safeTry . readFile) file
  let lookupLoc = maybe (\_ _ _ -> Nothing) (lookupLocation file)  mInput
  runIO (runSpecM spec) >>= fromSpecList . addLoctions lookupLoc

addLoctions :: (Int -> Int -> String -> Maybe Location) -> [SpecTree a] -> [SpecTree a]
addLoctions lookupLoc = map (fmap f) . enumerate
  where
    f :: ((Int, Int), Item a) -> Item a
    f ((n, total), item) = item {itemLocation = itemLocation item <|> lookupLoc n total (itemRequirement item)}

type EnumerateM = State [(String, Int)]

enumerate :: [SpecTree a] -> [Tree (ActionWith a) ((Int, Int), (Item a))]
enumerate tree = (mapM (traverse addPosition) tree >>= mapM (traverse addTotal)) `evalState` []
  where
    addPosition :: Item a -> EnumerateM (Int, Item a)
    addPosition item = (,) <$> getOccurrence (itemRequirement item) <*> pure item

    addTotal :: (Int, Item a) -> EnumerateM ((Int, Int), Item a)
    addTotal (n, item) = do
      total <- getTotal (itemRequirement item)
      return ((n, total), item)

    getTotal :: String -> EnumerateM Int
    getTotal requirement = do
      gets $ fromMaybe err . lookup requirement
      where
        err = error ("Test.Hspec.Discover.getTotal: No entry for requirement " ++ show requirement ++ "!")

    getOccurrence :: String -> EnumerateM Int
    getOccurrence requirement = do
      xs <- get
      let n = maybe 1 succ (lookup requirement xs)
      put ((requirement, n) : filter ((/= requirement) . fst) xs)
      return n

lookupLocation :: FilePath -> String -> Int -> Int -> String -> Maybe Location
lookupLocation file input n total requirement = loc
  where
    loc :: Maybe Location
    loc = Location file <$> line <*> pure 0 <*> pure BestEffort

    line :: Maybe Int
    line = case occurrences of
      xs | length xs == total -> Just (xs !! pred n)
      _ -> Nothing

    occurrences :: [Int]
    occurrences = map fst (filter p inputLines)
      where
        p :: (Int, String) -> Bool
        p = isInfixOf (show requirement) . snd

    inputLines :: [(Int, String)]
    inputLines = zip [1..] (lines input)
