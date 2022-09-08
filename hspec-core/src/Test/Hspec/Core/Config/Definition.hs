{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Config.Definition (
  Config(..)
, ColorMode(..)
, UnicodeMode(..)
, filterOr
, defaultConfig

, commandLineOnlyOptions
, formatterOptions
, smallCheckOptions
, quickCheckOptions
, runnerOptions

#ifdef TEST
, splitOn
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Example (Params(..), defaultParams)
import           Test.Hspec.Core.Format (Format, FormatConfig)
import           Test.Hspec.Core.Formatters.Pretty (pretty2)
import qualified Test.Hspec.Core.Formatters.V1 as V1
import qualified Test.Hspec.Core.Formatters.V2 as V2
import           Test.Hspec.Core.Util

import           GetOpt.Declarative


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
, configPrettyPrint :: Bool
, configPrettyPrintFunction :: Bool -> String -> String -> (String, String)
, configTimes :: Bool
, configAvailableFormatters :: [(String, FormatConfig -> IO Format)]
, configFormat :: Maybe (FormatConfig -> IO Format)
, configFormatter :: Maybe V1.Formatter -- ^ deprecated, use `configFormat` instead
, configHtmlOutput :: Bool
, configConcurrentJobs :: Maybe Int
}

defaultConfig :: Config
defaultConfig = Config {
  configIgnoreConfigFile = False
, configDryRun = False
, configFocusedOnly = False
, configFailOnEmpty = False
, configFailOnFocused = False
, configFailOnPending = False
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
, configPrettyPrint = True
, configPrettyPrintFunction = pretty2
, configTimes = False
, configAvailableFormatters = map (fmap V2.formatterToFormat) [
    ("checks", V2.checks)
  , ("specdoc", V2.specdoc)
  , ("progress", V2.progress)
  , ("failed-examples", V2.failed_examples)
  , ("silent", V2.silent)
  ]
, configFormat = Nothing
, configFormatter = Nothing
, configHtmlOutput = False
, configConcurrentJobs = Nothing
}

option :: String -> OptionSetter config -> String -> Option config
option name arg help = Option name Nothing arg help True

mkFlag :: String -> (Bool -> Config -> Config) -> String -> Option Config
mkFlag name setter = option name (Flag setter)

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
    mkOption "format" (Just 'f') (argument "FORMATTER" readFormatter setFormatter) helpForFormat
  , mkFlag "color" setColor "colorize the output"
  , mkFlag "unicode" setUnicode "output unicode"
  , mkFlag "diff" setDiff "show colorized diffs"
  , mkFlag "pretty" setPretty "try to pretty-print diff values"
  , mkFlag "times" setTimes "report times for individual spec items"
  , mkOptionNoArg "print-cpu-time" Nothing setPrintCpuTime "include used CPU time in summary"
  , printSlowItemsOption

    -- undocumented for now, as we probably want to change this to produce a
    -- standalone HTML report in the future
  , undocumented $ mkOptionNoArg "html" Nothing setHtml "produce HTML output"
  ]
  where
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

    setPretty :: Bool -> Config -> Config
    setPretty v config = config {configPrettyPrint = v}

    setTimes :: Bool -> Config -> Config
    setTimes v config = config {configTimes = v}

    setPrintCpuTime config = config {configPrintCpuTime = True}

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
      Just 0 -> Just (setter Nothing c)
      Just n -> Just (setter (Just n) c)
      Nothing -> Nothing

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
  deriving (Bounded, Enum)

allFailOnItems :: [FailOn]
allFailOnItems = [minBound .. maxBound]

showFailOn :: FailOn -> String
showFailOn item = case item of
  FailOnEmpty -> "empty"
  FailOnFocused -> "focused"
  FailOnPending -> "pending"

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
    mkFlag "dry-run" setDryRun "pretend that everything passed; don't verify anything"
  , mkFlag "focused-only" setFocusedOnly "do not run anything, unless there are focused spec items"

  , undocumented $ mkFlag "fail-on-focused" setFailOnFocused "fail on focused spec items"
  , undocumented $ mkFlag "fail-on-pending" setFailOnPending "fail on pending spec items"

  , mkOption    "fail-on" Nothing (argument "ITEMS" readFailOnItems (setFailOnItems True )) helpForFailOn
  , mkOption "no-fail-on" Nothing (argument "ITEMS" readFailOnItems (setFailOnItems False)) helpForFailOn
  , mkFlag "strict" setStrict $ "same as --fail-on=" <> showFailOnItems strict

  , mkFlag "fail-fast" setFailFast "abort on first failure"
  , mkFlag "randomize" setRandomize "randomize execution order"
  , mkOptionNoArg "rerun" (Just 'r') setRerun "rerun all examples that failed in the previous test run (only works in combination with --failure-report or in GHCi)"
  , option "failure-report" (argument "FILE" return setFailureReport) "read/write a failure report for use with --rerun"
  , mkOptionNoArg "rerun-all-on-success" Nothing setRerunAllOnSuccess "run the whole test suite after a previously failing rerun succeeds for the first time (only works in combination with --rerun)"
  , mkOption "jobs" (Just 'j') (argument "N" readMaxJobs setMaxJobs) "run at most N parallelizable tests simultaneously (default: number of available processors)"
  ]
  where
    strict = allFailOnItems

    readFailOnItems :: String -> Maybe [FailOn]
    readFailOnItems = mapM readFailOn . splitOn ','

    showFailOnItems :: [FailOn] -> String
    showFailOnItems = intercalate "," . map showFailOn

    helpForFailOn :: String
    helpForFailOn = unlines . flip map allFailOnItems $ \ item ->
      showFailOn item <> ": " <> help item
      where
        help item = case item of
          FailOnEmpty -> "fail if no spec items have been run"
          FailOnFocused -> "fail on focused spec items"
          FailOnPending -> "fail on pending spec items"

    setFailOnItems :: Bool -> [FailOn] -> Config -> Config
    setFailOnItems value = flip $ foldr (`setItem` value)
      where
        setItem item = case item of
          FailOnEmpty -> setFailOnEmpty
          FailOnFocused -> setFailOnFocused
          FailOnPending -> setFailOnPending

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
