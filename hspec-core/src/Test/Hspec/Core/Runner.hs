{-# LANGUAGE CPP #-}

-- |
-- Stability: provisional
module Test.Hspec.Core.Runner (
-- * Running a spec
  hspec
, runSpec

-- * Config
, Config (..)
, ColorMode (..)
, Path
, defaultConfig
, configAddFilter
, readConfig

-- * Summary
, Summary (..)
, isSuccess
, evaluateSummary

-- * Legacy
-- | The following primitives are deprecated.  Use `runSpec` instead.
, hspecWith
, hspecResult
, hspecWithResult

#ifdef TEST
, rerunAll
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Maybe
import           System.IO
import           System.Environment (getArgs, withArgs)
import           System.Exit
import qualified Control.Exception as E

import           System.Console.ANSI (hHideCursor, hShowCursor)
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Formatters
import           Test.Hspec.Core.Formatters.Internal
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheckUtil

import           Test.Hspec.Core.Runner.Eval

-- | Filter specs by given predicate.
--
-- The predicate takes a list of "describe" labels and a "requirement".
filterSpecs :: Config -> [EvalTree] -> [EvalTree]
filterSpecs c = go []
  where
    p :: Path -> Bool
    p path = (fromMaybe (const True) (configFilterPredicate c) path) &&
               not (fromMaybe (const False) (configSkipPredicate c) path)

    go :: [String] -> [EvalTree] -> [EvalTree]
    go groups = mapMaybe (goSpec groups)

    goSpecs :: [String] -> [EvalTree] -> ([EvalTree] -> b) -> Maybe b
    goSpecs groups specs ctor = case go groups specs of
      [] -> Nothing
      xs -> Just (ctor xs)

    goSpec :: [String] -> EvalTree -> Maybe (EvalTree)
    goSpec groups spec = case spec of
      Leaf item -> guard (p (groups, evalItemDescription item)) >> return spec
      Node group specs -> goSpecs (groups ++ [group]) specs (Node group)
      NodeWithCleanup action specs -> goSpecs groups specs (NodeWithCleanup action)

applyDryRun :: Config -> [SpecTree ()] -> [SpecTree ()]
applyDryRun c
  | configDryRun c = map (removeCleanup . fmap markSuccess)
  | otherwise = id
  where
    markSuccess :: Item () -> Item ()
    markSuccess item = item {itemExample = safeEvaluateExample (Result "" Success)}

    removeCleanup :: SpecTree () -> SpecTree ()
    removeCleanup spec = case spec of
      Node x xs -> Node x (map removeCleanup xs)
      NodeWithCleanup _ xs -> NodeWithCleanup (\() -> return ()) (map removeCleanup xs)
      leaf@(Leaf _) -> leaf

-- | Run a given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
--
-- /Note/: `hspec` handles command-line options and reads config files.  This
-- is not always desired.  Use `runSpec` if you need more control over these
-- aspects.
hspec :: Spec -> IO ()
hspec spec =
      getArgs
  >>= readConfig defaultConfig
  >>= doNotLeakCommandLineArgumentsToExamples . runSpec spec
  >>= evaluateSummary

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
hspecWith config spec = getArgs >>= readConfig config >>= doNotLeakCommandLineArgumentsToExamples . runSpec spec >>= evaluateSummary

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
hspecResult spec = getArgs >>= readConfig defaultConfig >>= doNotLeakCommandLineArgumentsToExamples . runSpec spec

-- | Run given spec with custom options and returns a summary of the test run.
--
-- /Note/: `hspecWithResult` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecWithResult :: Config -> Spec -> IO Summary
hspecWithResult config spec = getArgs >>= readConfig config >>= doNotLeakCommandLineArgumentsToExamples . runSpec spec

-- |
-- `runSpec` is the most basic primitive to run a spec. `hspec` is defined in
-- terms of @runSpec@:
--
-- @
-- hspec spec =
--       `getArgs`
--   >>= `readConfig` `defaultConfig`
--   >>= `withArgs` [] . runSpec spec
--   >>= `evaluateSummary`
-- @
runSpec :: Spec -> Config -> IO Summary
runSpec spec c_ = do
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
    normalMode c = runSpec_ c spec
    rerunAllMode c oldFailureReport = do
      summary <- runSpec_ c spec
      if rerunAll c oldFailureReport summary
        then runSpec spec c_
        else return summary

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

failFocusedItems :: Config -> Spec -> Spec
failFocusedItems config spec
  | configFailOnFocused config = mapSpecItem_ failFocused spec
  | otherwise = spec

focusSpec :: Config -> Spec -> Spec
focusSpec config spec
  | configFocusedOnly config = spec
  | otherwise = focus spec

runSpec_ :: Config -> Spec -> IO Summary
runSpec_ config spec = do
  withHandle config $ \h -> do
    let formatter = fromMaybe specdoc (configFormatter config)
        seed = (fromJust . configQuickCheckSeed) config
        qcArgs = configQuickCheckArgs config

    concurrentJobs <- case configConcurrentJobs config of
      Nothing -> getDefaultConcurrentJobs
      Just n -> return n

    useColor <- doesUseColor h config

    let
      focusedSpec = focusSpec config (failFocusedItems config spec)
      params = Params (configQuickCheckArgs config) (configSmallCheckDepth config)

    filteredSpec <- filterSpecs config . mapMaybe (toEvalTree params) . applyDryRun config <$> runSpecM focusedSpec

    (total, failures) <- withHiddenCursor useColor h $ do
      let
        formatConfig = FormatConfig {
          formatConfigHandle = h
        , formatConfigUseColor = useColor
        , formatConfigUseDiff = configDiff config
        , formatConfigHtmlOutput = configHtmlOutput config
        , formatConfigPrintCpuTime = configPrintCpuTime config
        , formatConfigUsedSeed =  seed
        }
        evalConfig = EvalConfig {
          evalConfigFormat = formatterToFormat formatter formatConfig
        , evalConfigConcurrentJobs = concurrentJobs
        , evalConfigFastFail = configFastFail config
        }
      runFormatter evalConfig filteredSpec

    dumpFailureReport config seed qcArgs failures
    return (Summary total (length failures))

toEvalTree :: Params -> SpecTree () -> Maybe EvalTree
toEvalTree params = go
  where
    go :: Tree (() -> c) (Item ()) -> Maybe (Tree c EvalItem)
    go t = case t of
      Node s xs -> Just $ Node s (mapMaybe go xs)
      NodeWithCleanup c xs -> Just $ NodeWithCleanup (c ()) (mapMaybe go xs)
      Leaf (Item requirement loc isParallelizable isFocused e) ->
        guard isFocused >> return (Leaf (EvalItem requirement loc (fromMaybe False isParallelizable) (e params $ ($ ()))))

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
#if !MIN_VERSION_base(4,11,0)
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
#else
instance Semigroup Summary where
  (Summary x1 x2) <> (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
#endif
