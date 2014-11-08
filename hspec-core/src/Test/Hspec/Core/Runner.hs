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
) where

import           Control.Monad
import           Control.Applicative
import           Data.Monoid
import           Data.Maybe
import           System.IO
import           System.Environment (getProgName, getArgs, withArgs)
import           System.Exit
import qualified Control.Exception as E

import           System.Console.ANSI (hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC
import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Compat (lookupEnv)
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Core.Formatters
import           Test.Hspec.Core.Formatters.Internal
import           Test.Hspec.FailureReport
import           Test.Hspec.Core.QuickCheckUtil

import           Test.Hspec.Core.Runner.Eval

-- | Filter specs by given predicate.
--
-- The predicate takes a list of "describe" labels and a "requirement".
filterSpecs :: Config -> [SpecTree a] -> [SpecTree a]
filterSpecs c = go []
  where
    p :: Path -> Bool
    p = fromMaybe (const True) (configFilterPredicate c)

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
    markSuccess item = item {itemExample = evaluateExample Success}

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
  unless (summaryFailures r == 0) exitFailure

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
hspecWithResult conf spec = do
  prog <- getProgName
  args <- getArgs
  c <- getConfig conf prog args >>= ensureSeed
  withArgs [] {- do not leak command-line arguments to examples -} $ withHandle c $ \h -> do
    let formatter = fromMaybe specdoc (configFormatter c)
        seed = (fromJust . configQuickCheckSeed) c
        qcArgs = configQuickCheckArgs c

    useColor <- doesUseColor h c

    filteredSpec <- filterSpecs c . applyDryRun c <$> runSpecM spec

    withHiddenCursor useColor h $
      runFormatM useColor (configHtmlOutput c) (configPrintCpuTime c) seed h $ do
        runFormatter useColor h c formatter filteredSpec `finally_` do
          failedFormatter formatter

        footerFormatter formatter

        -- dump failure report
        xs <- map failureRecordPath <$> getFailMessages
        liftIO $ writeFailureReport FailureReport {
            failureReportSeed = seed
          , failureReportMaxSuccess = QC.maxSuccess qcArgs
          , failureReportMaxSize = QC.maxSize qcArgs
          , failureReportMaxDiscardRatio = QC.maxDiscardRatio qcArgs
          , failureReportPaths = xs
          }

        Summary <$> getTotalCount <*> getFailCount
  where
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
