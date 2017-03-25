module Test.Hspec.Core.Options (
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
import           Control.Monad
import           Test.Hspec.Core.Compat

import           System.IO
import           System.Exit
import           System.Console.GetOpt

import           Test.Hspec.Core.Formatters
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

formatters :: [(String, Formatter)]
formatters = [
    ("specdoc", specdoc)
  , ("progress", progress)
  , ("failed-examples", failed_examples)
  , ("silent", silent)
  ]

formatHelp :: String
formatHelp = unlines (addLineBreaks "use a custom formatter; this can be one of:" ++ map (("   " ++) . fst) formatters)

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

addLineBreaks :: String -> [String]
addLineBreaks = lineBreaksAt 40

h :: String -> String
h = unlines . addLineBreaks

commandLineOptions :: [OptDescr (Result Maybe -> Result Maybe)]
commandLineOptions = [
    Option   []  ["help"]             (NoArg (const $ Right Nothing))     (h "display this help and exit")
  , Option   []  ["ignore-dot-hspec"] (NoArg setIgnoreConfigFile)         (h "do not read options from ~/.hspec and .hspec")
  , mkOption "m"  "match"             (Arg "PATTERN" return addMatch)     (h "only run examples that match given PATTERN")
  , mkOption []   "skip"              (Arg "PATTERN" return addSkip)      (h "skip examples that match given PATTERN")
  ]
  where
    setIgnoreConfigFile = set $ \config -> config {configIgnoreConfigFile = True}

configFileOptions :: Monad m => [OptDescr (Result m -> Result m)]
configFileOptions = [
    Option   []  ["color"]            (NoArg setColor)                    (h "colorize the output")
  , Option   []  ["no-color"]         (NoArg setNoColor)                  (h "do not colorize the output")
  , Option   []  ["diff"]             (NoArg setDiff)                     (h "show colorized diffs")
  , Option   []  ["no-diff"]          (NoArg setNoDiff)                   (h "do not show colorized diffs")
  , mkOption "f"  "format"            (Arg "FORMATTER" readFormatter setFormatter) formatHelp
  , mkOption "o"  "out"               (Arg "FILE" return setOutputFile)   (h "write output to a file instead of STDOUT")
  , mkOption []   "depth"             (Arg "N" readMaybe setDepth)        (h "maximum depth of generated test values for SmallCheck properties")
  , mkOption "a"  "qc-max-success"    (Arg "N" readMaybe setMaxSuccess)   (h "maximum number of successful tests before a QuickCheck property succeeds")
  , mkOption ""   "qc-max-size"       (Arg "N" readMaybe setMaxSize)      (h "size to use for the biggest test cases")
  , mkOption ""   "qc-max-discard"    (Arg "N" readMaybe setMaxDiscardRatio) (h "maximum number of discarded tests per successful test before giving up")
  , mkOption []   "seed"              (Arg "N" readMaybe setSeed)         (h "used seed for QuickCheck properties")
  , Option   []  ["print-cpu-time"]   (NoArg setPrintCpuTime)             (h "include used CPU time in summary")
  , Option   []  ["dry-run"]          (NoArg setDryRun)                   (h "pretend that everything passed; don't verify anything")
  , Option   []  ["fail-fast"]        (NoArg setFastFail)                 (h "abort on first failure")
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "rerun all examples that failed in the previous test run (only works in combination with --failure-report or in GHCi)")
  , mkOption []   "failure-report"    (Arg "FILE" return setFailureReport)(h "read/write a failure report for use with --rerun")
  , Option   []  ["rerun-all-on-success"] (NoArg setRerunAllOnSuccess)    (h "run the whole test suite after a previously failing rerun succeeds for the first time (only works in combination with --rerun)")
  , mkOption "j"  "jobs"              (Arg "N" readMaxJobs setMaxJobs)    (h "run at most N parallelizable tests simultaneously (default: number of available processors)")
  ]
  where
    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    readMaxJobs :: String -> Maybe Int
    readMaxJobs s = do
      n <- readMaybe s
      guard $ n > 0
      return n

    setFormatter :: Formatter -> Config -> Config
    setFormatter f c = c {configFormatter = Just f}

    setOutputFile :: String -> Config -> Config
    setOutputFile file c = c {configOutputFile = Right file}

    setFailureReport :: String -> Config -> Config
    setFailureReport file c = c {configFailureReport = Just file}

    setMaxJobs :: Int -> Config -> Config
    setMaxJobs n c = c {configConcurrentJobs = Just n}

    setPrintCpuTime = set $ \config -> config {configPrintCpuTime = True}
    setDryRun       = set $ \config -> config {configDryRun = True}
    setFastFail     = set $ \config -> config {configFastFail = True}
    setRerun        = set $ \config -> config {configRerun = True}
    setRerunAllOnSuccess = set $ \config -> config {configRerunAllOnSuccess = True}
    setColor        = set $ \config -> config {configColorMode = ColorAlways}
    setNoColor      = set $ \config -> config {configColorMode = ColorNever}
    setDiff         = set $ \config -> config {configDiff = True}
    setNoDiff       = set $ \config -> config {configDiff = False}

set :: Monad m => (Config -> Config) -> Either a (m Config) -> Either a (m Config)
set = liftM . liftM

documentedOptions :: [OptDescr (Result Maybe -> Result Maybe)]
documentedOptions = commandLineOptions ++ configFileOptions

undocumentedOptions :: [OptDescr (Result Maybe -> Result Maybe)]
undocumentedOptions = [
    -- for compatibility with test-framework
    mkOption [] "maximum-generated-tests" (Arg "NUMBER" readMaybe setMaxSuccess) "how many automated tests something like QuickCheck should try, by default"

    -- undocumented for now, as we probably want to change this to produce a
    -- standalone HTML report in the future
  , Option []  ["html"]                    (NoArg setHtml)                    "produce HTML output"

    -- now a noop
  , Option "v" ["verbose"]                 (NoArg id)                         "do not suppress output to stdout when evaluating examples"
  ]
  where
    setHtml :: Result Maybe -> Result Maybe
    setHtml = set $ \config -> config {configHtmlOutput = True}

recognizedOptions :: [OptDescr (Result Maybe -> Result Maybe)]
recognizedOptions = documentedOptions ++ undocumentedOptions

parseOptions :: Config -> String -> [ConfigFile] -> Maybe EnvVar -> [String] -> Either (ExitCode, String) Config
parseOptions config prog configFiles envVar args = do
      foldM (parseFileOptions prog) config configFiles
  >>= parseEnvVarOptions prog envVar
  >>= parseCommandLineOptions prog args

parseCommandLineOptions :: String -> [String] -> Config -> Either (ExitCode, String) Config
parseCommandLineOptions prog args config = case parse recognizedOptions config args of
  Right Nothing -> Left (ExitSuccess, usageInfo ("Usage: " ++ prog ++ " [OPTION]...\n\nOPTIONS") documentedOptions)
  Right (Just c) -> Right c
  Left err -> failure err
  where
    failure err = Left (ExitFailure 1, prog ++ ": " ++ err ++ "\nTry `" ++ prog ++ " --help' for more information.\n")

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
