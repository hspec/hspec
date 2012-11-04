module Test.Hspec.Runner (
  hspec
, hspecWith

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

-- | Evaluate and print the result of checking the specs examples.
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


-- | Create a document of the given spec and write it to stdout.
--
-- Exit the program with `exitFailure` if at least one example fails.
--
-- (see also `hspecWith`)
hspec :: Spec -> IO ()
hspec spec = do
  c <- getConfig
  withArgs [] {- do not leak command-line arguments to examples -} $ do
    r <- hspecWith c spec
    unless (summaryFailures r == 0) exitFailure

-- | Run given specs.  This is similar to `hspec`, but more flexible.
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
