{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,6,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

-- |
-- Stability: provisional
module Test.Hspec.Core.Runner (
-- * Running a spec
  hspec
, hspecWith
, hspecResult
, hspecWithResult

-- * Types
, Summary (..)
, Config (..)
, ColorMode (..)
, Path
, defaultConfig
, configAddFilter

#ifdef TEST
, rerunAll
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad
import           Data.Maybe
import           System.IO
import           System.Environment (getProgName, getArgs, withArgs)
import           System.Exit
import qualified Control.Exception as E
import           Control.Concurrent

import           System.Console.ANSI (hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC
import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Formatters
import           Test.Hspec.Core.Formatters.Internal
import qualified Test.Hspec.Core.Formatters.Internal as Formatter
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheckUtil

import           Test.Hspec.Core.Runner.Eval

-- | Filter specs by given predicate.
--
-- The predicate takes a list of "describe" labels and a "requirement".
filterSpecs :: Config -> [SpecTree a] -> [SpecTree a]
filterSpecs c = go []
  where
    p :: Path -> Bool
    p path = (fromMaybe (const True) (configFilterPredicate c) path) &&
               not (fromMaybe (const False) (configSkipPredicate c) path)

    go :: [String] -> [SpecTree a] -> [SpecTree a]
    go groups = mapMaybe (goSpec groups)

    goSpecs :: [String] -> [SpecTree a] -> ([SpecTree a] -> b) -> Maybe b
    goSpecs groups specs ctor = case go groups specs of
      [] -> Nothing
      xs -> Just (ctor xs)

    goSpec :: [String] -> SpecTree a -> Maybe (SpecTree a)
    goSpec groups spec = case spec of
      Leaf item -> guard (p (groups, itemRequirement item)) >> return spec
      Node group specs -> goSpecs (groups ++ [group]) specs (Node group)
      NodeWithCleanup action specs -> goSpecs groups specs (NodeWithCleanup action)

applyDryRun :: Config -> [SpecTree ()] -> [SpecTree ()]
applyDryRun c
  | configDryRun c = map (removeCleanup . fmap markSuccess)
  | otherwise = id
  where
    markSuccess :: Item () -> Item ()
    markSuccess item = item {itemExample = safeEvaluateExample Success}

    removeCleanup :: SpecTree () -> SpecTree ()
    removeCleanup spec = case spec of
      Node x xs -> Node x (map removeCleanup xs)
      NodeWithCleanup _ xs -> NodeWithCleanup (\() -> return ()) (map removeCleanup xs)
      leaf@(Leaf _) -> leaf

-- | Run given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
hspec :: Spec -> IO ()
hspec = hspecWith defaultConfig

-- Add a seed to given config if there is none.  That way the same seed is used
-- for all properties.  This helps with --seed and --rerun.
ensureSeed :: Config -> IO Config
ensureSeed c = case configQuickCheckSeed c of
  Nothing -> do
    seed <- newSeed
    return c {configQuickCheckSeed = Just (fromIntegral seed)}
  _       -> return c

-- | Run given spec with custom options.
-- This is similar to `hspec`, but more flexible.
hspecWith :: Config -> Spec -> IO ()
hspecWith conf spec = do
  r <- hspecWithResult conf spec
  unless (isSuccess r) exitFailure

isSuccess :: Summary -> Bool
isSuccess summary = summaryFailures summary == 0

-- | Run given spec and returns a summary of the test run.
--
-- /Note/: `hspecResult` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecResult :: Spec -> IO Summary
hspecResult = hspecWithResult defaultConfig

-- | Run given spec with custom options and returns a summary of the test run.
--
-- /Note/: `hspecWithResult` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecWithResult :: Config -> Spec -> IO Summary
hspecWithResult config spec = do
  prog <- getProgName
  args <- getArgs
  (oldFailureReport, c_) <- getConfig config prog args
  c <- ensureSeed c_
  if configRerunAllOnSuccess c
    -- With --rerun-all we may run the spec twice. For that reason GHC can not
    -- optimize away the spec tree. That means that the whole spec tree has to
    -- be constructed in memory and we loose constant space behavior.
    --
    -- By separating between rerunAllMode and normalMode here, we retain
    -- constant space behavior in normalMode.
    --
    -- see: https://github.com/hspec/hspec/issues/169
    then rerunAllMode c oldFailureReport
    else normalMode c
  where
    normalMode c = runSpec c spec
    rerunAllMode c oldFailureReport = do
      summary <- runSpec c spec
      if rerunAll c oldFailureReport summary
        then hspecWithResult config spec
        else return summary

runSpec :: Config -> Spec -> IO Summary
runSpec config spec = do
  doNotLeakCommandLineArgumentsToExamples $ withHandle config $ \h -> do
    let formatter = fromMaybe specdoc (configFormatter config)
        seed = (fromJust . configQuickCheckSeed) config
        qcArgs = configQuickCheckArgs config

    jobsSem <- newQSem =<< case configConcurrentJobs config of
      Nothing -> getDefaultConcurrentJobs
      Just maxJobs -> return maxJobs

    useColor <- doesUseColor h config

    filteredSpec <- filterSpecs config . applyDryRun config <$> runSpecM spec

    withHiddenCursor useColor h $
      runFormatM useColor (configDiff config) (configHtmlOutput config) (configPrintCpuTime config) seed h $ do
        runFormatter jobsSem useColor h config formatter filteredSpec `finally_` do
          Formatter.interpret $ failedFormatter formatter

        Formatter.interpret $ footerFormatter formatter

        xs <- map failureRecordPath <$> Formatter.interpret getFailMessages
        liftIO $ dumpFailureReport seed qcArgs xs

        Summary <$> Formatter.interpret getTotalCount <*> Formatter.interpret getFailCount

dumpFailureReport :: Integer -> QC.Args -> [Path] -> IO ()
dumpFailureReport seed qcArgs xs = do
  writeFailureReport FailureReport {
      failureReportSeed = seed
    , failureReportMaxSuccess = QC.maxSuccess qcArgs
    , failureReportMaxSize = QC.maxSize qcArgs
    , failureReportMaxDiscardRatio = QC.maxDiscardRatio qcArgs
    , failureReportPaths = xs
    }

doNotLeakCommandLineArgumentsToExamples :: IO a -> IO a
doNotLeakCommandLineArgumentsToExamples = withArgs []

withHiddenCursor :: Bool -> Handle -> IO a -> IO a
withHiddenCursor useColor h
  | useColor  = E.bracket_ (hHideCursor h) (hShowCursor h)
  | otherwise = id

doesUseColor :: Handle -> Config -> IO Bool
doesUseColor h c = case configColorMode c of
  ColorAuto  -> (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
  ColorNever -> return False
  ColorAlways -> return True

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle c action = case configOutputFile c of
  Left h -> action h
  Right path -> withFile path WriteMode action

rerunAll :: Config -> Maybe FailureReport -> Summary -> Bool
rerunAll _ Nothing _ = False
rerunAll config (Just oldFailureReport) summary =
     configRerunAllOnSuccess config
  && configRerun config
  && isSuccess summary
  && (not . null) (failureReportPaths oldFailureReport)

isDumb :: IO Bool
isDumb = maybe False (== "dumb") <$> lookupEnv "TERM"

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
