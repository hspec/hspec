{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | This module contains the runners that take a set of specs, evaluate their examples, and
-- report to a given handle.
--
module Test.Hspec.Runner (
  Specs
, hspec
, hspecB
, hHspec
, hHspecWithFormat
, toExitCode

, Summary (..)

-- * Deprecated functions
, hspecX
) where

import           Control.Monad (unless, (>=>))
import           Data.Monoid

import           Test.Hspec.Internal
import           Test.Hspec.Core (EvaluatedSpec, Specs)
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           System.IO
import           System.Exit

-- | Evaluate and print the result of checking the spec examples.
runFormatter :: Formatter -> Spec -> FormatM EvaluatedSpec
runFormatter formatter = go 0 [] . unSpec
  where
    go nesting groups (SpecGroup group xs) = do
      exampleGroupStarted formatter nesting group
      ys <- mapM (go (succ nesting) (group : groups)) xs
      return (SpecGroup group ys)
    go nesting groups (SpecExample requirement e) = do
      result <- liftIO $ safeEvaluateExample e
      case result of
        Success -> do
          increaseSuccessCount
          exampleSucceeded formatter nesting requirement
        Fail err -> do
          increaseFailCount
          exampleFailed  formatter nesting requirement err
          n <- getFailCount
          addFailMessage $ failureDetails groups requirement err n
        Pending reason -> do
          increasePendingCount
          examplePending formatter nesting requirement reason
      return (SpecExample requirement result)

failureDetails :: [String] -> String -> String -> Int -> String
failureDetails groups requirement err i =
  show i ++ ") " ++ groups_ ++ requirement ++ " FAILED" ++ err_
  where
    err_
      | null err  = ""
      | otherwise = "\n" ++ err
    groups_ = case groups of
      [x] -> x ++ " "
      _   -> concatMap (++ " - ") (reverse groups)


-- | Create a document of the given specs and write it to stdout.
--
-- Exit the program with `exitSuccess` if all examples passed, with
-- `exitFailure` otherwise.
hspec :: Specs -> IO ()
hspec = hspecB >=> (`unless` exitFailure)

-- | DEPRECATED: Use `hspec` instead.
hspecX :: Specs -> IO a
hspecX = hspecB >=> exitWith . toExitCode

-- | Create a document of the given specs and write it to stdout.
--
-- Return `True` if all examples passed, `False` otherwise.
hspecB :: Specs -> IO Bool
hspecB = fmap success . hHspec stdout
  where
    success :: Summary -> Bool
    success s = summaryFailures s == 0

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\h -> hHspec h specs)
--
hHspec :: Handle -> Specs -> IO Summary
hHspec h specs = do
  useColor <- hIsTerminalDevice h
  hHspecWithFormat specdoc useColor h specs

-- | Create a document of the given specs and write it to the given handle.
-- THIS IS LIKELY TO CHANGE
hHspecWithFormat :: Formatter -> Bool -> Handle -> Specs -> IO Summary
hHspecWithFormat formatter useColor h ss = fmap count $ runFormatM useColor h $ do
  specList <- mapM (runFormatter formatter) ss
  failedFormatter formatter
  footerFormatter formatter
  return specList
  where
    count :: [EvaluatedSpec] -> Summary
    count = mconcat . map f
      where
        f (SpecGroup _ xs)  = mconcat (map f xs)
        f (SpecExample _ x) = Summary 1 (if isFailure x then 1 else 0)

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
