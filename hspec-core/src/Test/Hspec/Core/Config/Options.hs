module Test.Hspec.Core.Config.Options (
  Config(..)
, ColorMode (..)
, defaultConfig
, filterOr
, parseOptions
, ConfigFile
, ignoreConfigFile
, envVarName
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           System.IO
import           System.Exit
import           System.Console.GetOpt

import           Test.Hspec.Core.Formatters
import           Test.Hspec.Core.Config.Util
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Example (Params(..), defaultParams)
import           Data.Functor.Identity
import           Data.Maybe

type ConfigFile = (FilePath, [String])

type EnvVar = [String]

envVarName :: String
envVarName = "HSPEC_OPTIONS"

data Config = Config {
  configIgnoreConfigFile :: Bool
, configDryRun :: Bool
, configFocusedOnly :: Bool
, configFailOnFocused :: Bool
, configPrintCpuTime :: Bool
, configFastFail :: Bool
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
, configSmallCheckDepth :: Int
, configColorMode :: ColorMode
, configDiff :: Bool
, configFormatter :: Maybe Formatter
, configHtmlOutput :: Bool
, configOutputFile :: Either Handle FilePath
, configConcurrentJobs :: Maybe Int
}

defaultConfig :: Config
defaultConfig = Config {
  configIgnoreConfigFile = False
, configDryRun = False
, configFocusedOnly = False
, configFailOnFocused = False
, configPrintCpuTime = False
, configFastFail = False
, configFailureReport = Nothing
, configRerun = False
, configRerunAllOnSuccess = False
, configFilterPredicate = Nothing
, configSkipPredicate = Nothing
, configQuickCheckSeed = Nothing
, configQuickCheckMaxSuccess = Nothing
, configQuickCheckMaxDiscardRatio = Nothing
, configQuickCheckMaxSize = Nothing
, configSmallCheckDepth = paramsSmallCheckDepth defaultParams
, configColorMode = ColorAuto
, configDiff = True
, configFormatter = Nothing
, configHtmlOutput = False
, configOutputFile = Left stdout
, configConcurrentJobs = Nothing
}

filterOr :: Maybe (Path -> Bool) -> Maybe (Path -> Bool) -> Maybe (Path -> Bool)
filterOr p1_ p2_ = case (p1_, p2_) of
  (Just p1, Just p2) -> Just $ \path -> p1 path || p2 path
  _ -> p1_ <|> p2_

addMatch :: String -> Config -> Config
addMatch s c = c {configFilterPredicate = Just (filterPredicate s) `filterOr` configFilterPredicate c}

addSkip :: String -> Config -> Config
addSkip s c = c {configSkipPredicate = Just (filterPredicate s) `filterOr` configSkipPredicate c}

setDepth :: Int -> Config -> Config
setDepth n c = c {configSmallCheckDepth = n}

setMaxSuccess :: Int -> Config -> Config
setMaxSuccess n c = c {configQuickCheckMaxSuccess = Just n}

setMaxSize :: Int -> Config -> Config
setMaxSize n c = c {configQuickCheckMaxSize = Just n}

setMaxDiscardRatio :: Int -> Config -> Config
setMaxDiscardRatio n c = c {configQuickCheckMaxDiscardRatio = Just n}

setSeed :: Integer -> Config -> Config
setSeed n c = c {configQuickCheckSeed = Just n}

data ColorMode = ColorAuto | ColorNever | ColorAlways
  deriving (Eq, Show)

type Result m = Either InvalidArgument (m Config)

data InvalidArgument = InvalidArgument String String

data Arg a = Arg {
  _argumentName   :: String
, _argumentParser :: String -> Maybe a
, _argumentSetter :: a -> Config -> Config
}

mkOption :: Monad m => [Char] -> String -> Arg a -> String -> OptDescr (Result m -> Result m)
mkOption shortcut name (Arg argName parser setter) help = Option shortcut [name] (ReqArg arg argName) help
  where
    arg input x = x >>= \c -> case parser input of
      Just n -> Right (setter n `liftM` c)
      Nothing -> Left (InvalidArgument name input)

mkFlag :: Monad m => String -> (Bool -> Config -> Config) -> String -> [OptDescr (Result m -> Result m)]
mkFlag name setter help = [
    Option [] [name] (NoArg $ set $ setter True) help
  , Option [] ["no-" ++ name] (NoArg $ set $ setter False) ("do not " ++ help)
  ]

commandLineOptions :: [OptDescr (Result Maybe -> Result Maybe)]
commandLineOptions = [
    Option [] ["help"] (NoArg (const $ Right Nothing)) "display this help and exit"
  , Option [] ["ignore-dot-hspec"] (NoArg setIgnoreConfigFile) "do not read options from ~/.hspec and .hspec"
  , mkOption "m" "match" (Arg "PATTERN" return addMatch) "only run examples that match given PATTERN"
  , mkOption [] "skip" (Arg "PATTERN" return addSkip) "skip examples that match given PATTERN"
  ]
  where
    setIgnoreConfigFile = set $ \config -> config {configIgnoreConfigFile = True}

formatterOptions :: Monad m => [OptDescr (Result m -> Result m)]
formatterOptions = concat [
    [mkOption "f" "format" (Arg "FORMATTER" readFormatter setFormatter) helpForFormat]
  , mkFlag "color" setColor "colorize the output"
  , mkFlag "diff" setDiff "show colorized diffs"
  , [Option [] ["print-cpu-time"] (NoArg setPrintCpuTime) "include used CPU time in summary"]
  ]
  where
    formatters :: [(String, Formatter)]
    formatters = [
        ("specdoc", specdoc)
      , ("progress", progress)
      , ("failed-examples", failed_examples)
      , ("silent", silent)
      ]

    helpForFormat :: String
    helpForFormat = "use a custom formatter; this can be one of " ++ (formatOrList $ map fst formatters)

    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    setFormatter :: Formatter -> Config -> Config
    setFormatter f c = c {configFormatter = Just f}

    setColor :: Bool -> Config -> Config
    setColor v config = config {configColorMode = if v then ColorAlways else ColorNever}

    setDiff :: Bool -> Config -> Config
    setDiff v config = config {configDiff = v}

    setPrintCpuTime = set $ \config -> config {configPrintCpuTime = True}

smallCheckOptions :: Monad m => [OptDescr (Result m -> Result m)]
smallCheckOptions = [
    mkOption [] "depth" (Arg "N" readMaybe setDepth) "maximum depth of generated test values for SmallCheck properties"
  ]

quickCheckOptions :: Monad m => [OptDescr (Result m -> Result m)]
quickCheckOptions = [
    mkOption "a" "qc-max-success" (Arg "N" readMaybe setMaxSuccess) "maximum number of successful tests before a QuickCheck property succeeds"
  , mkOption "" "qc-max-size" (Arg "N" readMaybe setMaxSize) "size to use for the biggest test cases"
  , mkOption "" "qc-max-discard" (Arg "N" readMaybe setMaxDiscardRatio) "maximum number of discarded tests per successful test before giving up"
  , mkOption [] "seed" (Arg "N" readMaybe setSeed) "used seed for QuickCheck properties"
  ]

runnerOptions :: Monad m => [OptDescr (Result m -> Result m)]
runnerOptions = concat [
    mkFlag "dry-run" setDryRun "pretend that everything passed; don't verify anything"
  , mkFlag "focused-only" setFocusedOnly "do not run anything, unless there are focused spec items"
  , mkFlag "fail-on-focused" setFailOnFocused "fail on focused spec items"
  , mkFlag "fail-fast" setFastFail "abort on first failure"
  ] ++ [
    Option "r" ["rerun"] (NoArg  setRerun) "rerun all examples that failed in the previous test run (only works in combination with --failure-report or in GHCi)"
  , mkOption [] "failure-report" (Arg "FILE" return setFailureReport) "read/write a failure report for use with --rerun"
  , Option [] ["rerun-all-on-success"] (NoArg setRerunAllOnSuccess) "run the whole test suite after a previously failing rerun succeeds for the first time (only works in combination with --rerun)"
  , mkOption "j" "jobs" (Arg "N" readMaxJobs setMaxJobs) "run at most N parallelizable tests simultaneously (default: number of available processors)"
  ]
  where
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

    setFailOnFocused :: Bool -> Config -> Config
    setFailOnFocused value config = config {configFailOnFocused = value}

    setFastFail :: Bool -> Config -> Config
    setFastFail value config = config {configFastFail = value}

    setRerun        = set $ \config -> config {configRerun = True}
    setRerunAllOnSuccess = set $ \config -> config {configRerunAllOnSuccess = True}

documentedConfigFileOptions :: Monad m => [(String, [OptDescr (Result m -> Result m)])]
documentedConfigFileOptions = [
    ("RUNNER OPTIONS", runnerOptions)
  , ("FORMATTER OPTIONS", formatterOptions)
  , ("OPTIONS FOR QUICKCHECK", quickCheckOptions)
  , ("OPTIONS FOR SMALLCHECK", smallCheckOptions)
  ]

documentedOptions :: [(String, [OptDescr (Result Maybe -> Result Maybe)])]
documentedOptions = ("OPTIONS", commandLineOptions) : documentedConfigFileOptions

configFileOptions :: Monad m => [OptDescr (Result m -> Result m)]
configFileOptions = (concat . map snd) documentedConfigFileOptions

set :: Monad m => (Config -> Config) -> Either a (m Config) -> Either a (m Config)
set = liftM . liftM

undocumentedOptions :: Monad m => [OptDescr (Result m -> Result m)]
undocumentedOptions = [
    -- for compatibility with test-framework
    mkOption [] "maximum-generated-tests" (Arg "NUMBER" readMaybe setMaxSuccess) "how many automated tests something like QuickCheck should try, by default"

    -- undocumented for now, as we probably want to change this to produce a
    -- standalone HTML report in the future
  , Option []  ["html"]                    (NoArg setHtml)                    "produce HTML output"

  , mkOption "o"  "out"                    (Arg "FILE" return setOutputFile)  "write output to a file instead of STDOUT"

    -- now a noop
  , Option "v" ["verbose"]                 (NoArg id)                         "do not suppress output to stdout when evaluating examples"
  ]
  where
    setHtml = set $ \config -> config {configHtmlOutput = True}

    setOutputFile :: String -> Config -> Config
    setOutputFile file c = c {configOutputFile = Right file}

recognizedOptions :: [OptDescr (Result Maybe -> Result Maybe)]
recognizedOptions = commandLineOptions ++ configFileOptions ++ undocumentedOptions

parseOptions :: Config -> String -> [ConfigFile] -> Maybe EnvVar -> [String] -> Either (ExitCode, String) Config
parseOptions config prog configFiles envVar args = do
      foldM (parseFileOptions prog) config configFiles
  >>= parseEnvVarOptions prog envVar
  >>= parseCommandLineOptions prog args

parseCommandLineOptions :: String -> [String] -> Config -> Either (ExitCode, String) Config
parseCommandLineOptions prog args config = case parse recognizedOptions config args of
  Right Nothing -> Left (ExitSuccess, usage)
  Right (Just c) -> Right c
  Left err -> failure err
  where
    failure err = Left (ExitFailure 1, prog ++ ": " ++ err ++ "\nTry `" ++ prog ++ " --help' for more information.\n")

    usage :: String
    usage = "Usage: " ++ prog ++ " [OPTION]...\n\n"
      ++ (intercalate "\n" $ map (uncurry mkUsageInfo) documentedOptions)

parseFileOptions :: String -> Config -> ConfigFile -> Either (ExitCode, String) Config
parseFileOptions prog config (name, args) =
  parseOtherOptions prog ("in config file " ++ name) args config

parseEnvVarOptions :: String -> (Maybe EnvVar) -> Config -> Either (ExitCode, String) Config
parseEnvVarOptions prog args =
  parseOtherOptions prog ("from environment variable " ++ envVarName) (fromMaybe [] args)

parseOtherOptions :: String -> String -> [String] -> Config -> Either (ExitCode, String) Config
parseOtherOptions prog source args config = case parse configFileOptions config args of
  Right (Identity c) -> Right c
  Left err -> failure err
  where
    failure err = Left (ExitFailure 1, prog ++ ": " ++ message)
      where
        message = unlines $ case lines err of
          [x] -> [x ++ " " ++ source]
          xs -> xs ++ [source]

parse :: Monad m => [OptDescr (Result m -> Result m)] -> Config -> [String] -> Either String (m Config)
parse options config args = case getOpt Permute options args of
  (opts, [], []) -> case foldl' (flip id) (Right $ return config) opts of
    Left (InvalidArgument name value) -> Left ("invalid argument `" ++ value ++ "' for `--" ++ name ++ "'")
    Right x -> Right x
  (_, _, err:_) -> Left (init err)
  (_, arg:_, _) -> Left ("unexpected argument `" ++ arg ++ "'")

ignoreConfigFile :: Config -> [String] -> IO Bool
ignoreConfigFile config args = do
  ignore <- lookupEnv "IGNORE_DOT_HSPEC"
  case ignore of
    Just _ -> return True
    Nothing -> case parse recognizedOptions config args of
      Right (Just c) -> return (configIgnoreConfigFile c)
      _ -> return False
