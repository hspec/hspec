-- |
-- Stability: provisional
module Test.Hspec.Runner (
-- * Running a spec
  hspec
, hspecWithFormatter
, hspecWith
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
import           System.Environment
import           System.Exit
import qualified Control.Exception as E

import           System.Console.ANSI (hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC
import           System.Random (newStdGen)
import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Util
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.FailureReport
import           Test.Hspec.Timer

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
      SpecItem requirement _ -> guard (p (groups, requirement)) >> return spec
      SpecGroup group specs     -> case goSpecs (groups ++ [group]) specs of
        [] -> Nothing
        xs -> Just (SpecGroup group xs)

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Bool -> Config -> Formatter -> [SpecTree] -> FormatM ()
runFormatter useColor c formatter specs = headerFormatter formatter >> zip [0..] specs `each` go []
  where
    -- like forM_, but respects --fast-fail
    each :: [a] -> (a -> FormatM ()) -> FormatM ()
    each []     _ = pure ()
    each (x:xs) f = do
      f x
      fails <- getFailCount
      unless (configFastFail c && fails /= 0) $ do
        xs `each` f

    eval :: IO Result -> FormatM (Either E.SomeException Result)
    eval
      | configDryRun c = \_ -> return (Right Success)
      | otherwise      = liftIO . safeTry . fmap forceResult

    go :: [String] -> (Int, SpecTree) -> FormatM ()
    go rGroups (n, SpecGroup group xs) = do
      exampleGroupStarted formatter n (reverse rGroups) group
      zip [0..] xs `each` go (group : rGroups)
      exampleGroupDone formatter
    go rGroups (_, SpecItem requirement example) = do
      progressHandler <- mkProgressHandler
      result <- eval (example $ Params (configQuickCheckArgs c) progressHandler)
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

        mkProgressHandler
          | useColor = do
              timer <- liftIO $ newTimer 0.05
              return $ \p -> do
                f <- timer
                when f $ do
                  exampleProgress formatter (configHandle c) path p
          | otherwise = return . const $ return ()

-- | Run given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
hspec :: Spec -> IO ()
hspec = hspecWith defaultConfig

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = f} spec

handleReRun :: Config -> IO Config
handleReRun c = do
  if configReRun c
    then do
      readFailureReport c
    else do
      return c

-- Add a StdGen to configQuickCheckArgs if there is none.  That way the same
-- seed is used for all properties.  This helps with --seed and --re-run.
ensureStdGen :: Config -> IO Config
ensureStdGen c = case QC.replay qcArgs of
  Nothing -> do
    stdGen <- newStdGen
    return c {configQuickCheckArgs = qcArgs {QC.replay = Just (stdGen, 0)}}
  _       -> return c
  where
    qcArgs = configQuickCheckArgs c

-- | Run given spec with custom options.
-- This is similar to `hspec`, but more flexible.
hspecWith :: Config -> Spec -> IO ()
hspecWith c_ spec = do
  c <- getConfig c_
  withArgs [] {- do not leak command-line arguments to examples -} $ do
    r <- hspecWithResult c spec
    unless (summaryFailures r == 0) exitFailure

-- | Run given spec with custom options and returns a summary of the test run.
--
-- /Note/: `hspecWithResult` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecWithResult :: Config -> Spec -> IO Summary
hspecWithResult c_ spec = do
  -- read failure report on --re-run
  c <- handleReRun c_ >>= ensureStdGen

  let formatter = configFormatter c
      h = configHandle c
      seed = (stdGenToInteger . fst . fromJust . QC.replay . configQuickCheckArgs) c

  useColor <- doesUseColor h c

  withHiddenCursor useColor h $
    runFormatM useColor (configHtmlOutput c) (configPrintCpuTime c) seed h $ do
      runFormatter useColor c formatter (maybe id filterSpecs (configFilterPredicate c) $ runSpecM spec) `finally_` do
        failedFormatter formatter

      footerFormatter formatter

      -- dump failure report
      xs <- map failureRecordPath <$> getFailMessages
      liftIO $ writeFailureReport (seed, xs)

      Summary <$> getTotalCount <*> getFailCount
  where
    withHiddenCursor :: Bool -> Handle -> IO a -> IO a
    withHiddenCursor useColor h
      | useColor  = E.bracket_ (hHideCursor h) (hShowCursor h)
      | otherwise = id

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
