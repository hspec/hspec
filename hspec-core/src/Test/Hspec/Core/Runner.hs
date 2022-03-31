{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Stability: provisional
module Test.Hspec.Core.Runner (
-- * Running a spec
{- |
To run a spec `hspec` performs a sequence of steps:

1. Evaluate a `Spec` to a forest of `SpecTree`s
1. Read config values from the command-line, config files and the process environment
1. Execute each spec item of the forest and report results to `stdout`
1. Exit with `exitFailure` if at least on spec item fails

The four primitives `evalSpec`, `readConfig`, `runSpecForest` and
`evaluateSummary` each perform one of these steps respectively.

`hspec` is defined in terms of these primitives:

@
hspec = `evalSpec` `defaultConfig` >=> \ (config, spec) ->
      `getArgs`
  >>= `readConfig` config
  >>= `withArgs` [] . `runSpecForest` spec
  >>= `evaluateSummary`
@

If you need more control over how a spec is run use these primitives individually.

-}
  hspec
, evalSpec
, runSpecForest

-- * Config
, Config (..)
, ColorMode (..)
, UnicodeMode(..)
, Path
, defaultConfig
, configAddFilter
, readConfig

-- * Summary
, Summary (..)
, isSuccess
, evaluateSummary

-- * Legacy
-- | The following primitives are deprecated.  Use `runSpecForest` instead.
, hspecWith
, hspecResult
, hspecWithResult
, runSpec

#ifdef TEST
, rerunAll
, specToEvalForest
, colorOutputSupported
, unicodeOutputSupported
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Maybe
import           System.IO
import           System.Environment (getArgs, withArgs)
import           System.Exit
import           Control.Arrow
import qualified Control.Exception as E
import           System.Random
import           Control.Monad.ST
import           Data.STRef
import           System.Console.ANSI (hSupportsANSI)

import           System.Console.ANSI (hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Spec hiding (pruneTree, pruneForest)
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Format (FormatConfig(..))
import qualified Test.Hspec.Core.Formatters.V1 as V1
import qualified Test.Hspec.Core.Formatters.V2 as V2
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Shuffle

import           Test.Hspec.Core.Runner.PrintSlowSpecItems
import           Test.Hspec.Core.Runner.Eval hiding (Tree(..))
import qualified Test.Hspec.Core.Runner.Eval as Eval


applyFilterPredicates :: Config -> [Tree c EvalItem] -> [Tree c EvalItem]
applyFilterPredicates c = filterForestWithLabels p
  where
    include :: Path -> Bool
    include = fromMaybe (const True) (configFilterPredicate c)

    skip :: Path -> Bool
    skip = fromMaybe (const False) (configSkipPredicate c)

    p :: [String] -> EvalItem -> Bool
    p groups item = include path && not (skip path)
      where
        path = (groups, evalItemDescription item)

applyDryRun :: Config -> [EvalItemTree] -> [EvalItemTree]
applyDryRun c
  | configDryRun c = bimapForest removeCleanup markSuccess
  | otherwise = id
  where
    removeCleanup :: IO () -> IO ()
    removeCleanup _ = return ()

    markSuccess :: EvalItem -> EvalItem
    markSuccess item = item {evalItemAction = \ _ -> return $ Result "" Success}

-- | Run a given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
--
-- /Note/: `hspec` handles command-line options and reads config files.  This
-- is not always desirable.  Use `evalSpec` and `runSpecForest` if you need
-- more control over these aspects.
hspec :: Spec -> IO ()
hspec = evalSpec defaultConfig >=> \ (config, spec) ->
      getArgs
  >>= readConfig config
  >>= doNotLeakCommandLineArgumentsToExamples . runSpecForest spec
  >>= evaluateSummary

-- |
-- Evaluate a `Spec` to a forest of `SpecTree`s.  This does not execute any
-- spec items, but it does run any IO that is used during spec construction
-- time (see `runIO`).
--
-- A `Spec` may modify a `Config` through `modifyConfig`.  These modifications
-- are applied to the given config (the first argument).
--
-- @since 2.10.0
evalSpec :: Config -> SpecWith a -> IO (Config, [SpecTree a])
evalSpec config spec = do
  (Endo f, forest) <- runSpecM spec
  return (f config, forest)

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
hspecWith defaults = hspecWithResult defaults >=> evaluateSummary

-- | `True` if the given `Summary` indicates that there were no
-- failures, `False` otherwise.
isSuccess :: Summary -> Bool
isSuccess summary = summaryFailures summary == 0

-- | Exit with `exitFailure` if the given `Summary` indicates that there was at
-- least one failure.
evaluateSummary :: Summary -> IO ()
evaluateSummary summary = unless (isSuccess summary) exitFailure

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
hspecWithResult defaults = evalSpec defaults >=> \ (config, spec) ->
      getArgs
  >>= readConfig config
  >>= doNotLeakCommandLineArgumentsToExamples . runSpecForest spec

-- |
-- /Note/: `runSpec` is deprecated. It ignores any modifications applied
-- through `modifyConfig`.  Use `evalSpec` and `runSpecForest` instead.
runSpec :: Spec -> Config -> IO Summary
runSpec spec config = evalSpec defaultConfig spec >>= flip runSpecForest config . snd

-- |
-- `runSpecForest` is the most basic primitive to run a spec. `hspec` is
-- defined in terms of @runSpecForest@:
--
-- @
-- hspec = `evalSpec` `defaultConfig` >=> \ (config, spec) ->
--       `getArgs`
--   >>= `readConfig` config
--   >>= `withArgs` [] . runSpecForest spec
--   >>= `evaluateSummary`
-- @
--
-- @since 2.10.0
runSpecForest :: [SpecTree ()] -> Config -> IO Summary
runSpecForest spec c_ = do
  oldFailureReport <- readFailureReportOnRerun c_

  c <- ensureSeed (applyFailureReport oldFailureReport c_)

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
    normalMode c = runSpecForest_ c spec
    rerunAllMode c oldFailureReport = do
      summary <- runSpecForest_ c spec
      if rerunAll c oldFailureReport summary
        then runSpecForest spec c_
        else return summary

runSpecForest_ :: Config -> [SpecTree ()] -> IO Summary
runSpecForest_ config spec = runEvalTree config (specToEvalForest config spec)

failFocused :: Item a -> Item a
failFocused item = item {itemExample = example}
  where
    failure = Failure Nothing (Reason "item is focused; failing due to --fail-on-focused")
    example
      | itemIsFocused item = \ params hook p -> do
          Result info status <- itemExample item params hook p
          return $ Result info $ case status of
            Success -> failure
            Pending _ _ -> failure
            Failure{} -> status
      | otherwise = itemExample item

failFocusedItems :: Config -> [SpecTree a] -> [SpecTree a]
failFocusedItems config spec
  | configFailOnFocused config = map (fmap failFocused) spec
  | otherwise = spec

focusSpec :: Config -> [SpecTree a] -> [SpecTree a]
focusSpec config spec
  | configFocusedOnly config = spec
  | otherwise = focusForest spec

runEvalTree :: Config -> [EvalTree] -> IO Summary
runEvalTree config spec = do
  let
      seed = (fromJust . configQuickCheckSeed) config
      qcArgs = configQuickCheckArgs config
      !numberOfItems = countSpecItems spec

  concurrentJobs <- case configConcurrentJobs config of
    Nothing -> getDefaultConcurrentJobs
    Just n -> return n

  useColor <- colorOutputSupported (configColorMode config) (hSupportsANSI stdout)
  outputUnicode <- unicodeOutputSupported (configUnicodeMode config) stdout

  results <- withHiddenCursor useColor stdout $ do
    let
      formatConfig = FormatConfig {
        formatConfigUseColor = useColor
      , formatConfigOutputUnicode = outputUnicode
      , formatConfigUseDiff = configDiff config
      , formatConfigPrettyPrint = configPrettyPrint config
      , formatConfigPrintTimes = configTimes config
      , formatConfigHtmlOutput = configHtmlOutput config
      , formatConfigPrintCpuTime = configPrintCpuTime config
      , formatConfigUsedSeed = seed
      , formatConfigExpectedTotalCount = numberOfItems
      }

      formatter = fromMaybe (V2.formatterToFormat V2.checks) (configFormat config <|> V1.formatterToFormat <$> configFormatter config)

    format <- maybe id printSlowSpecItems (configPrintSlowItems config) <$> formatter formatConfig

    let
      evalConfig = EvalConfig {
        evalConfigFormat = format
      , evalConfigConcurrentJobs = concurrentJobs
      , evalConfigFailFast = configFailFast config
      }
    runFormatter evalConfig spec

  let failures = filter resultItemIsFailure results

  dumpFailureReport config seed qcArgs (map fst failures)

  return Summary {
    summaryExamples = length results
  , summaryFailures = length failures
  }

specToEvalForest :: Config -> [SpecTree ()] -> [EvalTree]
specToEvalForest config =
      failFocusedItems config
  >>> focusSpec config
  >>> toEvalItemForest params
  >>> applyDryRun config
  >>> applyFilterPredicates config
  >>> randomize
  >>> pruneForest
  where
    seed = (fromJust . configQuickCheckSeed) config
    params = Params (configQuickCheckArgs config) (configSmallCheckDepth config)
    randomize
      | configRandomize config = randomizeForest seed
      | otherwise = id

pruneForest :: [Tree c a] -> [Eval.Tree c a]
pruneForest = mapMaybe pruneTree

pruneTree :: Tree c a -> Maybe (Eval.Tree c a)
pruneTree node = case node of
  Node group xs -> Eval.Node group <$> prune xs
  NodeWithCleanup loc action xs -> Eval.NodeWithCleanup loc action <$> prune xs
  Leaf item -> Just (Eval.Leaf item)
  where
    prune = nonEmpty . pruneForest

type EvalItemTree = Tree (IO ()) EvalItem

toEvalItemForest :: Params -> [SpecTree ()] -> [EvalItemTree]
toEvalItemForest params = bimapForest withUnit toEvalItem . filterForest itemIsFocused
  where
    toEvalItem :: Item () -> EvalItem
    toEvalItem (Item requirement loc isParallelizable _isFocused e) = EvalItem requirement loc (fromMaybe False isParallelizable) (e params withUnit)

    withUnit :: ActionWith () -> IO ()
    withUnit action = action ()

dumpFailureReport :: Config -> Integer -> QC.Args -> [Path] -> IO ()
dumpFailureReport config seed qcArgs xs = do
  writeFailureReport config FailureReport {
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

colorOutputSupported :: ColorMode -> IO Bool -> IO Bool
colorOutputSupported mode isTerminalDevice = case mode of
  ColorAuto  -> (||) <$> githubActions <*> colorTerminal
  ColorNever -> return False
  ColorAlways -> return True
  where
    githubActions :: IO Bool
    githubActions = lookupEnv "GITHUB_ACTIONS" <&> (== Just "true")

    colorTerminal :: IO Bool
    colorTerminal = (&&) <$> (not <$> noColor) <*> isTerminalDevice

    noColor :: IO Bool
    noColor = lookupEnv "NO_COLOR" <&> (/= Nothing)

unicodeOutputSupported :: UnicodeMode -> Handle -> IO Bool
unicodeOutputSupported mode h = case mode of
  UnicodeAuto -> (== Just "UTF-8") . fmap show <$> hGetEncoding h
  UnicodeNever -> return False
  UnicodeAlways -> return True

rerunAll :: Config -> Maybe FailureReport -> Summary -> Bool
rerunAll _ Nothing _ = False
rerunAll config (Just oldFailureReport) summary =
     configRerunAllOnSuccess config
  && configRerun config
  && isSuccess summary
  && (not . null) (failureReportPaths oldFailureReport)

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
#if MIN_VERSION_base(4,11,0)
instance Semigroup Summary where
#endif
  (Summary x1 x2)
#if MIN_VERSION_base(4,11,0)
    <>
#else
    `mappend`
#endif
    (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)

randomizeForest :: Integer -> [Tree c a] -> [Tree c a]
randomizeForest seed t = runST $ do
  ref <- newSTRef (mkStdGen $ fromIntegral seed)
  shuffleForest ref t

countSpecItems :: [Eval.Tree c a] -> Int
countSpecItems = getSum . foldMap (foldMap . const $ Sum 1)
