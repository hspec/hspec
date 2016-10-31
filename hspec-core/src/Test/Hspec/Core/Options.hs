module Test.Hspec.Core.Options (
  Config(..)
, ColorMode (..)
, defaultConfig
, filterOr
, parseOptions
, ConfigFile
, ignoreConfigFile
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

type ConfigFile = (FilePath, [String])

data Config = Config {
  configIgnoreConfigFile :: Bool
, configDryRun :: Bool
, configPrintCpuTime :: Bool
, configFastFail :: Bool
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
, configDiff = False
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

type Result = Either NoConfig Config

data NoConfig = Help | InvalidArgument String String

data Arg a = Arg {
  _argumentName   :: String
, _argumentParser :: String -> Maybe a
, _argumentSetter :: a -> Config -> Config
}

mkOption :: [Char] -> String -> Arg a -> String -> OptDescr (Result -> Result)
mkOption shortcut name (Arg argName parser setter) help = Option shortcut [name] (ReqArg arg argName) help
  where
    arg :: String -> Result -> Result
    arg input x = x >>= \c -> case parser input of
      Just n -> Right (setter n c)
      Nothing -> Left (InvalidArgument name input)

addLineBreaks :: String -> [String]
addLineBreaks = lineBreaksAt 40

h :: String -> String
h = unlines . addLineBreaks

commandLineOptions :: [OptDescr (Result -> Result)]
commandLineOptions = [
    Option   []  ["help"]             (NoArg (const $ Left Help))         (h "display this help and exit")
  , Option   []  ["ignore-dot-hspec"] (NoArg setIgnoreConfigFile)         (h "do not read options from ~/.hspec and .hspec")
  , mkOption "m"  "match"             (Arg "PATTERN" return addMatch)     (h "only run examples that match given PATTERN")
  , mkOption []   "skip"              (Arg "PATTERN" return addSkip)      (h "skip examples that match given PATTERN")
  ]
  where
    setIgnoreConfigFile x = x >>= \c -> return c {configIgnoreConfigFile = True}

configFileOptions :: [OptDescr (Result -> Result)]
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
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "rerun all examples that failed in the previous test run (only works in GHCi)")
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

    setMaxJobs :: Int -> Config -> Config
    setMaxJobs n c = c {configConcurrentJobs = Just n}

    setPrintCpuTime x = x >>= \c -> return c {configPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {configDryRun = True}
    setFastFail     x = x >>= \c -> return c {configFastFail = True}
    setRerun        x = x >>= \c -> return c {configRerun = True}
    setRerunAllOnSuccess x = x >>= \c -> return c {configRerunAllOnSuccess = True}
    setColor        x = x >>= \c -> return c {configColorMode = ColorAlways}
    setNoColor      x = x >>= \c -> return c {configColorMode = ColorNever}
    setDiff         x = x >>= \c -> return c {configDiff = True}
    setNoDiff       x = x >>= \c -> return c {configDiff = False}

documentedOptions :: [OptDescr (Result -> Result)]
documentedOptions = commandLineOptions ++ configFileOptions

undocumentedOptions :: [OptDescr (Result -> Result)]
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
    setHtml :: Result -> Result
    setHtml x = x >>= \c -> return c {configHtmlOutput = True}

recognizedOptions :: [OptDescr (Result -> Result)]
recognizedOptions = documentedOptions ++ undocumentedOptions

parseOptions :: Config -> String -> [ConfigFile] -> [String] -> Either (ExitCode, String) Config
parseOptions config prog configFiles args = do
  foldM (parseFileOptions prog) config configFiles >>= parseCommandLineOptions prog args

parseCommandLineOptions :: String -> [String] -> Config -> Either (ExitCode, String) Config
parseCommandLineOptions prog args config = case parse recognizedOptions config args of
  Right Nothing -> Left (ExitSuccess, usageInfo ("Usage: " ++ prog ++ " [OPTION]...\n\nOPTIONS") documentedOptions)
  Right (Just c) -> Right c
  Left err -> failure err
  where
    failure msg = Left (ExitFailure 1, prog ++ ": " ++ msg ++ "\nTry `" ++ prog ++ " --help' for more information.\n")

parseFileOptions :: String -> Config -> ConfigFile -> Either (ExitCode, String) Config
parseFileOptions prog config (name, args) = case parse configFileOptions config args of
  Right Nothing -> error "this should never happen"
  Right (Just c) -> Right c
  Left err -> failure err
  where
    failure msg = Left (ExitFailure 1, prog ++ ": " ++ msg ++ " in config file " ++ name ++ "\n")

parse :: [OptDescr (Result -> Result)] -> Config -> [String] -> Either String (Maybe Config)
parse options config args = case getOpt Permute options args of
  (opts, [], []) -> case foldl' (flip id) (Right config) opts of
    Left Help -> Right Nothing
    Left (InvalidArgument flag value) -> Left ("invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'")
    Right x -> Right (Just x)
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
