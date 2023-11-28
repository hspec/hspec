{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Config.Definition (
  Config(..)
, ColorMode(..)
, UnicodeMode(..)
, filterOr
, mkDefaultConfig

, commandLineOnlyOptions
, formatterOptions
, smallCheckOptions
, quickCheckOptions
, runnerOptions

, flag
, option
, argument

, setConfigAnnotation
, getConfigAnnotation
, addCustomOption
, addSpecTransformation

#ifdef TEST
, splitOn
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (openTempFile, hClose)
import           System.Process (system)

import           Test.Hspec.Core.Annotations (Annotations)
import qualified Test.Hspec.Core.Annotations as Annotations
import           Test.Hspec.Core.Tree (SpecTree)
import           Test.Hspec.Core.Example (Params(..), defaultParams)
import           Test.Hspec.Core.Format (Format, FormatConfig)
import           Test.Hspec.Core.Formatters.Pretty (pretty2)
import qualified Test.Hspec.Core.Formatters.V1.Monad as V1
import           Test.Hspec.Core.Util

import           GetOpt.Declarative

setConfigAnnotation :: Typeable value => value -> Config -> Config
setConfigAnnotation value config = config { configAnnotations = Annotations.setValue value $ configAnnotations config }

getConfigAnnotation :: Typeable value => Config -> Maybe value
getConfigAnnotation = Annotations.getValue . configAnnotations

addCustomOption :: String -> Option Config -> Config -> Config
addCustomOption section opt config = config { configCustomOptions = (section, [opt]) : configCustomOptions config }

addSpecTransformation :: (Config -> [SpecTree ()] -> [SpecTree ()]) -> Config -> Config
addSpecTransformation f config = config { configMapSpecForest = \ c -> f c . configMapSpecForest config c }

data ColorMode = ColorAuto | ColorNever | ColorAlways
  deriving (Eq, Show)

data UnicodeMode = UnicodeAuto | UnicodeNever | UnicodeAlways
  deriving (Eq, Show)

data Config = Config {
  configIgnoreConfigFile :: Bool
, configDryRun :: Bool
, configFocusedOnly :: Bool
, configFailOnEmpty :: Bool
, configFailOnFocused :: Bool
, configFailOnPending :: Bool
, configFailOnEmptyDescription :: Bool
, configPrintSlowItems :: Maybe Int
, configPrintCpuTime :: Bool
, configFailFast :: Bool
, configRandomize :: Bool
, configFailureReport :: Maybe FilePath
, configRerun :: Bool
, configRerunAllOnSuccess :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configFilterPredicate :: Maybe (Path -> Bool)
, configSkipPredicate :: Maybe (Path -> Bool)
, configQuickCheckSeed :: Maybe Integer
, configQuickCheckMaxSuccess :: Maybe Int
, configQuickCheckMaxDiscardRatio :: Maybe Int
, configQuickCheckMaxSize :: Maybe Int
, configQuickCheckMaxShrinks :: Maybe Int
, configSmallCheckDepth :: Maybe Int
, configColorMode :: ColorMode
, configUnicodeMode :: UnicodeMode
, configDiff :: Bool
, configDiffContext :: Maybe Int

-- |
-- An action that is used to print diffs.  The first argument is the value of
-- `configDiffContext`.  The remaining two arguments are the @expected@ and
-- @actual@ value.
--
-- @since 2.10.6
, configExternalDiff :: Maybe (Maybe Int -> String -> String -> IO ())

, configPrettyPrint :: Bool
, configPrettyPrintFunction :: Bool -> String -> String -> (String, String)
, configFormatException :: SomeException -> String -- ^ @since 2.11.5
, configTimes :: Bool
, configExpertMode :: Bool -- ^ @since 2.11.2
, configAvailableFormatters :: [(String, FormatConfig -> IO Format)] -- ^ @since 2.9.0
, configFormat :: Maybe (FormatConfig -> IO Format)
, configFormatter :: Maybe V1.Formatter
, configHtmlOutput :: Bool
, configConcurrentJobs :: Maybe Int
, configCustomOptions :: [(String, [Option Config])] -- ^ @since 2.12.0
, configAnnotations :: Annotations -- ^ @since 2.12.0
, configMapSpecForest :: Config -> [SpecTree ()] -> [SpecTree ()] -- ^ @since 2.12.0
}
{-# DEPRECATED configFormatter "Use [@useFormatter@](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Formatters-V1.html#v:useFormatter) instead." #-}

mkDefaultConfig :: [(String, FormatConfig -> IO Format)] -> Config
mkDefaultConfig formatters = Config {
  configIgnoreConfigFile = False
, configDryRun = False
, configFocusedOnly = False
, configFailOnEmpty = False
, configFailOnFocused = False
, configFailOnPending = False
, configFailOnEmptyDescription = False
, configPrintSlowItems = Nothing
, configPrintCpuTime = False
, configFailFast = False
, configRandomize = False
, configFailureReport = Nothing
, configRerun = False
, configRerunAllOnSuccess = False
, configFilterPredicate = Nothing
, configSkipPredicate = Nothing
, configQuickCheckSeed = Nothing
, configQuickCheckMaxSuccess = Nothing
, configQuickCheckMaxDiscardRatio = Nothing
, configQuickCheckMaxSize = Nothing
, configQuickCheckMaxShrinks = Nothing
, configSmallCheckDepth = paramsSmallCheckDepth defaultParams
, configColorMode = ColorAuto
, configUnicodeMode = UnicodeAuto
, configDiff = True
, configDiffContext = Just defaultDiffContext
, configExternalDiff = Nothing
, configPrettyPrint = True
, configPrettyPrintFunction = pretty2
, configFormatException = formatExceptionWith show
, configTimes = False
, configExpertMode = False
, configAvailableFormatters = formatters
, configFormat = Nothing
, configFormatter = Nothing
, configHtmlOutput = False
, configConcurrentJobs = Nothing
, configAnnotations = mempty
, configCustomOptions = []
, configMapSpecForest = \ _ -> id
}

defaultDiffContext :: Int
defaultDiffContext = 3

externalDiff :: String -> String -> String -> IO ()
externalDiff command expected actual = do
  tmp <- getTemporaryDirectory
  withTempFile tmp "hspec-expected" expected $ \ expectedFile -> do
    withTempFile tmp "hspec-actual" actual $ \ actualFile -> do
      void . system $ unwords [command, expectedFile, actualFile]

withTempFile :: FilePath -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempFile dir file contents action = do
  bracket (openTempFile dir file) (removeFile . fst) $ \ (path, h) -> do
    hClose h
    writeFile path contents
    action path

option :: String -> OptionSetter config -> String -> Option config
option name arg help = Option name Nothing arg help True

flag :: String -> (Bool -> Config -> Config) -> String -> Option Config
flag name setter = option name (Flag setter)

mkOptionNoArg :: String -> Maybe Char -> (Config -> Config) -> String -> Option Config
mkOptionNoArg name shortcut setter help = Option name shortcut (NoArg setter) help True

mkOption :: String -> Maybe Char -> OptionSetter Config -> String -> Option Config
mkOption name shortcut arg help = Option name shortcut arg help True

undocumented :: Option config -> Option config
undocumented opt = opt {optionDocumented = False}

argument :: String -> (String -> Maybe a) -> (a -> Config -> Config) -> OptionSetter Config
argument name parser setter = Arg name $ \ input c -> flip setter c <$> parser input

formatterOptions :: [(String, FormatConfig -> IO Format)] -> [Option Config]
formatterOptions formatters = [
    mkOption "format" (Just 'f') (argument "NAME" readFormatter setFormatter) helpForFormat
  , flag "color" setColor "colorize the output"
  , flag "unicode" setUnicode "output unicode"
  , flag "diff" setDiff "show colorized diffs"
  , option "diff-context" (argument "N" readDiffContext setDiffContext) $ unlines [
        "output N lines of diff context (default: " <> show defaultDiffContext <> ")"
      , "use a value of 'full' to see the full context"
      ]
  , option "diff-command" (argument "CMD" return setDiffCommand) "use an external diff command\nexample: --diff-command=\"git diff\""
  , flag "pretty" setPretty "try to pretty-print diff values"
  , mkOptionNoArg "show-exceptions" Nothing setShowException "use `show` when formatting exceptions"
  , mkOptionNoArg "display-exceptions" Nothing setDisplayException "use `displayException` when formatting exceptions"

  , flag "times" setTimes "report times for individual spec items"
  , mkOptionNoArg "print-cpu-time" Nothing setPrintCpuTime "include used CPU time in summary"
  , printSlowItemsOption
  , flag "expert" setExpertMode "be less verbose"

    -- undocumented for now, as we probably want to change this to produce a
    -- standalone HTML report in the future
  , undocumented $ mkOptionNoArg "html" Nothing setHtml "produce HTML output"
  ]
  where
    setDiffCommand :: String -> Config -> Config
    setDiffCommand command config = config {
      configExternalDiff = case strip command of
        "" -> Nothing
        _ -> Just $ \ _context -> externalDiff command
    }

    setHtml config = config {configHtmlOutput = True}

    helpForFormat :: String
    helpForFormat = "use a custom formatter; this can be one of " ++ (formatOrList $ map fst formatters)

    readFormatter :: String -> Maybe (FormatConfig -> IO Format)
    readFormatter = (`lookup` formatters)

    setFormatter :: (FormatConfig -> IO Format) -> Config -> Config
    setFormatter f c = c {configFormat = Just f}

    setColor :: Bool -> Config -> Config
    setColor v config = config {configColorMode = if v then ColorAlways else ColorNever}

    setUnicode :: Bool -> Config -> Config
    setUnicode v config = config {configUnicodeMode = if v then UnicodeAlways else UnicodeNever}

    setDiff :: Bool -> Config -> Config
    setDiff v config = config {configDiff = v}

    readDiffContext :: String -> Maybe (Maybe Int)
    readDiffContext input = case input of
      "full" -> Just Nothing
      _ -> case readMaybe input of
        Nothing -> Nothing
        mn -> Just (find (>= 0) mn)

    setDiffContext :: Maybe Int -> Config -> Config
    setDiffContext value c = c { configDiffContext = value }

    setPretty :: Bool -> Config -> Config
    setPretty v config = config {configPrettyPrint = v}

    setShowException :: Config -> Config
    setShowException config = config {configFormatException = formatExceptionWith show}

    setDisplayException :: Config -> Config
    setDisplayException config = config {configFormatException = formatExceptionWith displayException}

    setTimes :: Bool -> Config -> Config
    setTimes v config = config {configTimes = v}

    setPrintCpuTime config = config {configPrintCpuTime = True}

    setExpertMode :: Bool -> Config -> Config
    setExpertMode v config = config {configExpertMode = v}

printSlowItemsOption :: Option Config
printSlowItemsOption = Option name (Just 'p') (OptArg "N" arg) "print the N slowest spec items (default: 10)" True
  where
    name = "print-slow-items"

    setter :: Maybe Int -> Config -> Config
    setter v c = c {configPrintSlowItems = v}

    arg :: Maybe String -> Config -> Maybe Config
    arg = maybe (Just . (setter $ Just 10)) parseArg

    parseArg :: String -> Config -> Maybe Config
    parseArg input c = case readMaybe input of
      Nothing -> Nothing
      mn -> Just (setter (find (> 0) mn) c)

smallCheckOptions :: [Option Config]
smallCheckOptions = [
    option "depth" (argument "N" readMaybe setDepth) "maximum depth of generated test values for SmallCheck properties"
  ]

setDepth :: Int -> Config -> Config
setDepth n c = c {configSmallCheckDepth = Just n}

quickCheckOptions :: [Option Config]
quickCheckOptions = [
    Option "qc-max-success" (Just 'a') (argument "N" readMaybe setMaxSuccess) "maximum number of successful tests before a QuickCheck property succeeds" True
  , option "qc-max-discard" (argument "N" readMaybe setMaxDiscardRatio) "maximum number of discarded tests per successful test before giving up"
  , option "qc-max-size" (argument "N" readMaybe setMaxSize) "size to use for the biggest test cases"
  , option "qc-max-shrinks" (argument "N" readMaybe setMaxShrinks) "maximum number of shrinks to perform before giving up (a value of 0 turns shrinking off)"
  , option "seed" (argument "N" readMaybe setSeed) "used seed for QuickCheck properties"

    -- for compatibility with test-framework
  , undocumented $ option "maximum-generated-tests" (argument "NUMBER" readMaybe setMaxSuccess) "how many automated tests something like QuickCheck should try, by default"
  ]

setMaxSuccess :: Int -> Config -> Config
setMaxSuccess n c = c {configQuickCheckMaxSuccess = Just n}

setMaxDiscardRatio :: Int -> Config -> Config
setMaxDiscardRatio n c = c {configQuickCheckMaxDiscardRatio = Just n}

setMaxSize :: Int -> Config -> Config
setMaxSize n c = c {configQuickCheckMaxSize = Just n}

setMaxShrinks :: Int -> Config -> Config
setMaxShrinks n c = c {configQuickCheckMaxShrinks = Just n}

setSeed :: Integer -> Config -> Config
setSeed n c = c {configQuickCheckSeed = Just n}

data FailOn =
    FailOnEmpty
  | FailOnFocused
  | FailOnPending
  | FailOnEmptyDescription
  deriving (Bounded, Enum)

allFailOnItems :: [FailOn]
allFailOnItems = [minBound .. maxBound]

showFailOn :: FailOn -> String
showFailOn item = case item of
  FailOnEmpty -> "empty"
  FailOnFocused -> "focused"
  FailOnPending -> "pending"
  FailOnEmptyDescription -> "empty-description"

readFailOn :: String -> Maybe FailOn
readFailOn = (`lookup` items)
  where
    items = map (showFailOn &&& id) allFailOnItems

splitOn :: Char -> String -> [String]
splitOn sep = go
  where
    go xs =  case break (== sep) xs of
      ("", "") -> []
      (y, "") -> [y]
      (y, _ : ys) -> y : go ys

runnerOptions :: [Option Config]
runnerOptions = [
    flag "dry-run" setDryRun "pretend that everything passed; don't verify anything"
  , flag "focused-only" setFocusedOnly "do not run anything, unless there are focused spec items"

  , undocumented $ flag "fail-on-focused" setFailOnFocused "fail on focused spec items"
  , undocumented $ flag "fail-on-pending" setFailOnPending "fail on pending spec items"

  , mkOption    "fail-on" Nothing (argument "ITEMS" readFailOnItems (setFailOnItems True )) helpForFailOn
  , mkOption "no-fail-on" Nothing (argument "ITEMS" readFailOnItems (setFailOnItems False)) helpForFailOn
  , flag "strict" setStrict $ "same as --fail-on=" <> showFailOnItems strict

  , flag "fail-fast" setFailFast "abort on first failure"
  , flag "randomize" setRandomize "randomize execution order"
  , mkOptionNoArg "rerun" (Just 'r') setRerun "rerun all examples that failed in the previous test run (only works in combination with --failure-report or in GHCi)"
  , option "failure-report" (argument "FILE" return setFailureReport) "read/write a failure report for use with --rerun"
  , mkOptionNoArg "rerun-all-on-success" Nothing setRerunAllOnSuccess "run the whole test suite after a previously failing rerun succeeds for the first time (only works in combination with --rerun)"
  , mkOption "jobs" (Just 'j') (argument "N" readMaxJobs setMaxJobs) "run at most N parallelizable tests simultaneously (default: number of available processors)"
  ]
  where
    strict = [FailOnFocused, FailOnPending]

    readFailOnItems :: String -> Maybe [FailOn]
    readFailOnItems = mapM readFailOn . splitOn ','

    showFailOnItems :: [FailOn] -> String
    showFailOnItems = intercalate "," . map showFailOn

    helpForFailOn :: String
    helpForFailOn = unlines . flip map allFailOnItems $ \ item ->
      showFailOn item <> ": " <> help item
      where
        help item = case item of
          FailOnEmpty -> "fail if all spec items have been filtered"
          FailOnFocused -> "fail on focused spec items"
          FailOnPending -> "fail on pending spec items"
          FailOnEmptyDescription -> "fail on empty descriptions"

    setFailOnItems :: Bool -> [FailOn] -> Config -> Config
    setFailOnItems value = flip $ foldr (`setItem` value)
      where
        setItem item = case item of
          FailOnEmpty -> setFailOnEmpty
          FailOnFocused -> setFailOnFocused
          FailOnPending -> setFailOnPending
          FailOnEmptyDescription -> setFailOnEmptyDescription

    readMaxJobs :: String -> Maybe Int
    readMaxJobs s = do
      n <- readMaybe s
      guard $ n > 0
      return n

    setFailureReport :: String -> Config -> Config
    setFailureReport file c = c {configFailureReport = Just file}

    setMaxJobs :: Int -> Config -> Config
    setMaxJobs n c = c {configConcurrentJobs = Just n}

    setDryRun :: Bool -> Config -> Config
    setDryRun value config = config {configDryRun = value}

    setFocusedOnly :: Bool -> Config -> Config
    setFocusedOnly value config = config {configFocusedOnly = value}

    setFailOnEmpty :: Bool -> Config -> Config
    setFailOnEmpty value config = config {configFailOnEmpty = value}

    setFailOnFocused :: Bool -> Config -> Config
    setFailOnFocused value config = config {configFailOnFocused = value}

    setFailOnPending :: Bool -> Config -> Config
    setFailOnPending value config = config {configFailOnPending = value}

    setFailOnEmptyDescription :: Bool -> Config -> Config
    setFailOnEmptyDescription value config = config {configFailOnEmptyDescription = value}

    setStrict :: Bool -> Config -> Config
    setStrict = (`setFailOnItems` strict)

    setFailFast :: Bool -> Config -> Config
    setFailFast value config = config {configFailFast = value}

    setRandomize :: Bool -> Config -> Config
    setRandomize value config = config {configRandomize = value}

    setRerun config = config {configRerun = True}
    setRerunAllOnSuccess config = config {configRerunAllOnSuccess = True}

commandLineOnlyOptions :: [Option Config]
commandLineOnlyOptions = [
    mkOptionNoArg "ignore-dot-hspec" Nothing setIgnoreConfigFile "do not read options from ~/.hspec and .hspec"
  , mkOption "match" (Just 'm') (argument "PATTERN" return addMatch) "only run examples that match given PATTERN"
  , option "skip" (argument "PATTERN" return addSkip) "skip examples that match given PATTERN"
  ]
  where
    setIgnoreConfigFile config = config {configIgnoreConfigFile = True}

addMatch :: String -> Config -> Config
addMatch s c = c {configFilterPredicate = Just (filterPredicate s) `filterOr` configFilterPredicate c}

addSkip :: String -> Config -> Config
addSkip s c = c {configSkipPredicate = Just (filterPredicate s) `filterOr` configSkipPredicate c}

filterOr :: Maybe (Path -> Bool) -> Maybe (Path -> Bool) -> Maybe (Path -> Bool)
filterOr p1_ p2_ = case (p1_, p2_) of
  (Just p1, Just p2) -> Just $ \path -> p1 path || p2 path
  _ -> p1_ <|> p2_

formatOrList :: [String] -> String
formatOrList xs = case xs of
  [] -> ""
  x : ys -> (case ys of
    [] -> x
    _ : [] -> x ++ " or "
    _ : _ : _ -> x ++ ", ") ++ formatOrList ys
