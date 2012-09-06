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

, Config (..)
, defaultConfig

-- * Deprecated functions
, hspecX
) where

import           Control.Monad (unless, (>=>))
import           Control.Applicative
import           Data.Monoid

import           Test.Hspec.Internal
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           System.IO
import           System.Exit

-- | Evaluate and print the result of checking the spec examples.
runFormatter :: Config -> Formatter -> Spec -> FormatM ()
runFormatter c formatter = go 0 []
  where
    go :: Int -> [String] -> Spec -> FormatM ()
    go nesting groups (SpecGroup group xs) = do
      exampleGroupStarted formatter nesting group
      mapM_ (go (succ nesting) (group : groups)) xs
    go nesting groups (SpecExample requirement e) = do
      result <- liftIO $ safeEvaluateExample (e c)
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
hspec specs = getConfig >>= (`hspecB_` specs) >>= (`unless` exitFailure)

{-# DEPRECATED hspecX "use hspec instead" #-}
hspecX :: Specs -> IO a
hspecX = hspecB >=> exitWith . toExitCode

-- | Create a document of the given specs and write it to stdout.
--
-- Return `True` if all examples passed, `False` otherwise.
hspecB :: Specs -> IO Bool
hspecB = hspecB_ defaultConfig

hspecB_ :: Config -> Specs -> IO Bool
hspecB_ c = fmap success . hHspec_ c stdout
  where
    success :: Summary -> Bool
    success s = summaryFailures s == 0

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\h -> hHspec h specs)
--
hHspec :: Handle -> Specs -> IO Summary
hHspec = hHspec_ defaultConfig

hHspec_ :: Config -> Handle -> Specs -> IO Summary
hHspec_ c h specs = hHspecWithFormat c specdoc h specs

-- | Create a document of the given specs and write it to the given handle.
-- THIS IS LIKELY TO CHANGE
hHspecWithFormat :: Config -> Formatter -> Handle -> Specs -> IO Summary
hHspecWithFormat c formatter h ss = do
  useColor <- doesUseColor h c
  runFormatM useColor h $ do
    mapM_ (runFormatter c formatter) ss
    failedFormatter formatter
    footerFormatter formatter
    Summary <$> getTotalCount <*> getFailCount

doesUseColor :: Handle -> Config -> IO Bool
doesUseColor h c = case configColorMode c of
  ColorAuto  -> hIsTerminalDevice h
  ColorNever -> return False
  ColorAlway -> return True

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
