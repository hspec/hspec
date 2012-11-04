-- |
-- Stability: provisional
module Test.Hspec.Runner (
-- * Running a spec
  hspec
, hspecWith

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
import           System.Environment
import           System.Exit
import           System.IO.Silently (silence)

import           Test.Hspec.Util (Path, safeEvaluate)
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.FailureReport

-- | Filter specs by given predicate.
--
-- The predicate takes a list of "describe" labels and a "requirement".
filterSpecs :: (Path -> Bool) -> [SpecTree] -> [SpecTree]
filterSpecs p = goSpecs []
  where
    goSpecs :: [String] -> [SpecTree] -> [SpecTree]
    goSpecs groups = catMaybes . map (goSpec groups)

    goSpec :: [String] -> SpecTree -> Maybe SpecTree
    goSpec groups spec = case spec of
      SpecExample requirement _ -> guard (p (groups, requirement)) >> return spec
      SpecGroup group specs     -> case goSpecs (groups ++ [group]) specs of
        [] -> Nothing
        xs -> Just (SpecGroup group xs)

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Config -> Formatter -> [SpecTree] -> FormatM ()
runFormatter c formatter specs = headerFormatter formatter >> mapM_ (go []) (zip [0..] specs)
  where
    silence_
      | configVerbose c = id
      | otherwise       = silence

    go :: [String] -> (Int, SpecTree) -> FormatM ()
    go rGroups (n, SpecGroup group xs) = do
      exampleGroupStarted formatter n (reverse rGroups) group
      mapM_ (go (group : rGroups)) (zip [0..] xs)
      exampleGroupDone formatter
    go rGroups (_, SpecExample requirement example) = do
      result <- (liftIO . safeEvaluate . silence_) (example $ configParams c)
      case result of
        Right Success -> do
          increaseSuccessCount
          exampleSucceeded formatter path
        Right (Pending reason) -> do
          increasePendingCount
          examplePending formatter path reason

        Right (Fail err) -> failed (Right err)
        Left e           -> failed (Left  e)
      where
        path = (groups, requirement)
        groups = reverse rGroups
        failed err = do
          increaseFailCount
          addFailMessage path err
          exampleFailed  formatter path err


-- | Run given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
--
-- (see also `hspecWith`)
hspec :: Spec -> IO ()
hspec spec = do
  c <- getConfig
  withArgs [] {- do not leak command-line arguments to examples -} $ do
    r <- hspecWith c spec
    unless (summaryFailures r == 0) exitFailure

-- | Run given spec with custom options.
-- This is similar to `hspec`, but more flexible.
--
-- /Note/: `hspecWith` does not exit with `exitFailure` on failing spec items.
-- If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecWith :: Config -> Spec -> IO Summary
hspecWith c_ spec = do
  -- read failure report on --re-run
  c <- if configReRun c_
    then do
      readFailureReport c_
    else do
      return c_

  let formatter = configFormatter c
      h = configHandle c

  useColor <- doesUseColor h c
  runFormatM useColor (configHtmlOutput c) h $ do
    runFormatter c formatter (maybe id filterSpecs (configFilterPredicate c) $ runSpecM spec)
    failedFormatter formatter
    footerFormatter formatter

    -- dump failure report
    xs <- map failureRecordPath <$> getFailMessages
    liftIO $ writeFailureReport (show xs)

    Summary <$> getTotalCount <*> getFailCount
  where
    doesUseColor :: Handle -> Config -> IO Bool
    doesUseColor h c = case configColorMode c of
      ColorAuto  -> hIsTerminalDevice h
      ColorNever -> return False
      ColorAlway -> return True

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
