{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
`evaluateResult` each perform one of these steps respectively.

`hspec` is defined in terms of these primitives:

@
hspec = `evalSpec` `defaultConfig` >=> \\ (config, spec) ->
      `getArgs`
  >>= `readConfig` config
  >>= `withArgs` [] . `runSpecForest` spec
  >>= `evaluateResult`
@

If you need more control over how a spec is run use these primitives individually.

-}
  hspec
, evalSpec
, runSpecForest
, evaluateResult

-- * Config
, Config (..)
, ColorMode (..)
, UnicodeMode(..)
, Path
, defaultConfig
, registerFormatter
, registerDefaultFormatter
, configAddFilter
, readConfig

-- * Result

-- ** Spec Result
, Test.Hspec.Core.Runner.Result.SpecResult
, Test.Hspec.Core.Runner.Result.specResultItems
, Test.Hspec.Core.Runner.Result.specResultSuccess

-- ** Result Item
, Test.Hspec.Core.Runner.Result.ResultItem
, Test.Hspec.Core.Runner.Result.resultItemPath
, Test.Hspec.Core.Runner.Result.resultItemStatus
, Test.Hspec.Core.Runner.Result.resultItemIsFailure

-- ** Result Item Status
, Test.Hspec.Core.Runner.Result.ResultItemStatus(..)

-- * Legacy
-- | The following primitives are deprecated.  Use `runSpecForest` instead.
, hspecWith
, hspecResult
, hspecWithResult
, runSpec

-- ** Summary
, Summary (..)
, toSummary
, isSuccess
, evaluateSummary

-- * Re-exports
, Spec
, SpecWith

#ifdef TEST
, rerunAll
, specToEvalForest
, colorOutputSupported
, unicodeOutputSupported
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           NonEmpty (nonEmpty)
import           System.IO
import           System.Environment (getArgs, withArgs)
import           System.Exit (exitFailure)
import           System.Random
import           Control.Monad.ST
import           Data.STRef

import           System.Console.ANSI (hSupportsANSI, hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Spec hiding (pruneTree, pruneForest)
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Format (Format, FormatConfig(..))
import qualified Test.Hspec.Core.Formatters.V1 as V1
import qualified Test.Hspec.Core.Formatters.V2 as V2
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Shuffle

import           Test.Hspec.Core.Runner.PrintSlowSpecItems
import           Test.Hspec.Core.Runner.Eval hiding (Tree(..))
import qualified Test.Hspec.Core.Runner.Eval as Eval
import           Test.Hspec.Core.Runner.Result

-- |
-- Make a formatter available for use with @--format@.
--
-- @since 2.10.5
registerFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
registerFormatter formatter config = config { configAvailableFormatters = formatter : configAvailableFormatters config }

-- |
-- Make a formatter available for use with @--format@ and use it by default.
--
-- @since 2.10.5
registerDefaultFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
registerDefaultFormatter formatter@(_, format) config = (registerFormatter formatter config) { configFormat = Just format }

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
    removeCleanup _ = pass

    markSuccess :: EvalItem -> EvalItem
    markSuccess item = item {evalItemAction = \ _ -> return (0, Result "" Success)}

-- | Run a given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
--
-- /Note/: `hspec` handles command-line options and reads config files.  This
-- is not always desirable.  Use `evalSpec` and `runSpecForest` if you need
-- more control over these aspects.
hspec :: Spec -> IO ()
hspec = hspecWith defaultConfig

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
hspecWith defaults = hspecWithSpecResult defaults >=> evaluateResult

-- | Exit with `exitFailure` if the given `Summary` indicates that there was at
-- least one failure.
evaluateSummary :: Summary -> IO ()
evaluateSummary summary = unless (isSuccess summary) exitFailure

evaluateResult :: SpecResult -> IO ()
evaluateResult result = unless (specResultSuccess result) exitFailure

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
hspecWithResult config = fmap toSummary . hspecWithSpecResult config

hspecWithSpecResult :: Config -> Spec -> IO SpecResult
hspecWithSpecResult defaults = evalSpec defaults >=> \ (config, spec) ->
      getArgs
  >>= readConfig config
  >>= doNotLeakCommandLineArgumentsToExamples . runSpecForest spec

-- |
-- /Note/: `runSpec` is deprecated. It ignores any modifications applied
-- through `modifyConfig`.  Use `evalSpec` and `runSpecForest` instead.
runSpec :: Spec -> Config -> IO Summary
runSpec spec config = evalSpec defaultConfig spec >>= fmap toSummary . flip runSpecForest config . snd

-- |
-- `runSpecForest` is the most basic primitive to run a spec. `hspec` is
-- defined in terms of @runSpecForest@:
--
-- @
-- hspec = `evalSpec` `defaultConfig` >=> \\ (config, spec) ->
--       `getArgs`
--   >>= `readConfig` config
--   >>= `withArgs` [] . runSpecForest spec
--   >>= `evaluateResult`
-- @
--
-- @since 2.10.0
runSpecForest :: [SpecTree ()] -> Config -> IO SpecResult
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
      result <- runSpecForest_ c spec
      if rerunAll c oldFailureReport result
        then runSpecForest spec c_
        else return result

mapItem :: (Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapItem f = map (fmap f)

failFocusedItems :: Config -> [SpecTree a] -> [SpecTree a]
failFocusedItems config
  | configFailOnFocused config = mapItem failFocused
  | otherwise = id

failFocused :: forall a. Item a -> Item a
failFocused item = item {itemExample = example}
  where
    failure :: ResultStatus
    failure = Failure Nothing (Reason "item is focused; failing due to --fail-on=focused")

    example :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
    example
      | itemIsFocused item = \ params hook p -> do
          Result info status <- itemExample item params hook p
          return $ Result info $ case status of
            Success -> failure
            Pending _ _ -> failure
            Failure{} -> status
      | otherwise = itemExample item

failPendingItems :: Config -> [SpecTree a] -> [SpecTree a]
failPendingItems config
  | configFailOnPending config = mapItem failPending
  | otherwise = id

failPending :: forall a. Item a -> Item a
failPending item = item {itemExample = example}
  where
    example :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
    example params hook p = do
      Result info status <- itemExample item params hook p
      return $ Result info $ case status of
        Pending loc _ -> Failure loc (Reason "item is pending; failing due to --fail-on=pending")
        _ -> status

focusSpec :: Config -> [SpecTree a] -> [SpecTree a]
focusSpec config spec
  | configFocusedOnly config = spec
  | otherwise = focusForest spec

runSpecForest_ :: Config -> [SpecTree ()] -> IO SpecResult
runSpecForest_ config spec = do
  let
    filteredSpec = specToEvalForest config spec
    seed = (fromJust . configQuickCheckSeed) config
    qcArgs = configQuickCheckArgs config
    !numberOfItems = countEvalItems filteredSpec

  when (configFailOnEmpty config && numberOfItems == 0) $ do
    when (countSpecItems spec /= 0) $ do
      die "all spec items have been filtered; failing due to --fail-on=empty"

  concurrentJobs <- case configConcurrentJobs config of
    Nothing -> getDefaultConcurrentJobs
    Just n -> return n

  (reportProgress, useColor) <- colorOutputSupported (configColorMode config) (hSupportsANSI stdout)
  outputUnicode <- unicodeOutputSupported (configUnicodeMode config) stdout

  results <- fmap toSpecResult . withHiddenCursor reportProgress stdout $ do
    let
      formatConfig = FormatConfig {
        formatConfigUseColor = useColor
      , formatConfigReportProgress = reportProgress
      , formatConfigOutputUnicode = outputUnicode
      , formatConfigUseDiff = configDiff config
      , formatConfigDiffContext = configDiffContext config
      , formatConfigExternalDiff = if configDiff config then ($ configDiffContext config) <$> configExternalDiff config else Nothing
      , formatConfigPrettyPrint = configPrettyPrint config
      , formatConfigPrettyPrintFunction = if configPrettyPrint config then Just (configPrettyPrintFunction config outputUnicode) else Nothing
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
    runFormatter evalConfig filteredSpec

  let
    failures :: [Path]
    failures = map resultItemPath $ filter resultItemIsFailure $ specResultItems results

  dumpFailureReport config seed qcArgs failures

  return results

specToEvalForest :: Config -> [SpecTree ()] -> [EvalTree]
specToEvalForest config =
      failFocusedItems config
  >>> failPendingItems config
  >>> focusSpec config
  >>> toEvalItemForest params
  >>> applyDryRun config
  >>> applyFilterPredicates config
  >>> randomize
  >>> pruneForest
  where
    seed :: Integer
    seed = (fromJust . configQuickCheckSeed) config

    params :: Params
    params = Params (configQuickCheckArgs config) (configSmallCheckDepth config)

    randomize :: [Tree c a] -> [Tree c a]
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
toEvalItemForest params = bimapForest id toEvalItem . filterForest itemIsFocused
  where
    toEvalItem :: Item () -> EvalItem
    toEvalItem (Item requirement loc isParallelizable _isFocused e) = EvalItem {
      evalItemDescription = requirement
    , evalItemLocation = loc
    , evalItemConcurrency = if isParallelizable == Just True then Concurrent else Sequential
    , evalItemAction = \ progress -> measure $ e params withUnit progress
    }

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
withHiddenCursor reportProgress h
  | reportProgress  = bracket_ (hHideCursor h) (hShowCursor h)
  | otherwise = id

colorOutputSupported :: ColorMode -> IO Bool -> IO (Bool, Bool)
colorOutputSupported mode isTerminalDevice = do
  github <- githubActions
  buildkite <- lookupEnv "BUILDKITE" <&> (== Just "true")
  useColor <- case mode of
    ColorAuto  -> (github ||) <$> colorTerminal
    ColorNever -> return False
    ColorAlways -> return True
  let reportProgress = not github && not buildkite && useColor
  return (reportProgress, useColor)
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

rerunAll :: Config -> Maybe FailureReport -> SpecResult -> Bool
rerunAll config mOldFailureReport result = case mOldFailureReport of
  Nothing -> False
  Just oldFailureReport ->
       configRerunAllOnSuccess config
    && configRerun config
    && specResultSuccess result
    && (not . null) (failureReportPaths oldFailureReport)

randomizeForest :: Integer -> [Tree c a] -> [Tree c a]
randomizeForest seed t = runST $ do
  ref <- newSTRef (mkStdGen $ fromIntegral seed)
  shuffleForest ref t

countEvalItems :: [Eval.Tree c a] -> Int
countEvalItems = getSum . foldMap (foldMap . const $ Sum 1)

countSpecItems :: [Tree c a] -> Int
countSpecItems = getSum . foldMap (foldMap . const $ Sum 1)
