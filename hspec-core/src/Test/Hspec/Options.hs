module Test.Hspec.Options (
  Config(..)
, ColorMode (..)
, defaultConfig
, filterOr
, parseOptions
) where

import           Prelude ()
import           Test.Hspec.Compat

import           System.IO
import           System.Exit
import           System.Console.GetOpt

import           Test.Hspec.Core.Formatters
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Example (Params(..), defaultParams)

data Config = Config {
  configDryRun :: Bool
, configPrintCpuTime :: Bool
, configFastFail :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configRerun :: Bool
, configFilterPredicate :: Maybe (Path -> Bool)
, configSkipPredicate :: Maybe (Path -> Bool)
, configQuickCheckSeed :: Maybe Integer
, configQuickCheckMaxSuccess :: Maybe Int
, configQuickCheckMaxDiscardRatio :: Maybe Int
, configQuickCheckMaxSize :: Maybe Int
, configSmallCheckDepth :: Int
, configColorMode :: ColorMode
, configFormatter :: Maybe Formatter
, configHtmlOutput :: Bool
, configOutputFile :: Either Handle FilePath
, configMaxParallelJobs :: Maybe Int
}

defaultConfig :: Config
defaultConfig = Config {
  configDryRun = False
, configPrintCpuTime = False
, configFastFail = False
, configRerun = False
, configFilterPredicate = Nothing
, configSkipPredicate = Nothing
, configQuickCheckSeed = Nothing
, configQuickCheckMaxSuccess = Nothing
, configQuickCheckMaxDiscardRatio = Nothing
, configQuickCheckMaxSize = Nothing
, configSmallCheckDepth = paramsSmallCheckDepth defaultParams
, configColorMode = ColorAuto
, configFormatter = Nothing
, configHtmlOutput = False
, configOutputFile = Left stdout
, configMaxParallelJobs = Nothing
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
addLineBreaks = lineBreaksAt 44

options :: [OptDescr (Result -> Result)]
options = [
    Option   []  ["help"]             (NoArg (const $ Left Help))         (h "display this help and exit")
  , mkOption "m"  "match"             (Arg "PATTERN" return addMatch)     (h "only run examples that match given PATTERN")
  , mkOption []   "skip"              (Arg "PATTERN" return addSkip)      (h "skip examples that match given PATTERN")
  , Option   []  ["color"]            (NoArg setColor)                    (h "colorize the output")
  , Option   []  ["no-color"]         (NoArg setNoColor)                  (h "do not colorize the output")
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
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "rerun all examples that failed in the previously test run (only works in GHCi)")
  , mkOption "j"  "jobs"              (Arg "N" readMaybe setMaxJobs)      (h "run at most N tests simultaneously (default: unlimited)")
  , Option   []  ["unlimited-jobs"]   (NoArg clearMaxJobs)                (h "allow an unlimited number of jobs to run simultaneously (this is the default)")
  ]
  where
    h = unlines . addLineBreaks

    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    setFormatter :: Formatter -> Config -> Config
    setFormatter f c = c {configFormatter = Just f}

    setOutputFile :: String -> Config -> Config
    setOutputFile file c = c {configOutputFile = Right file}

    setMaxJobs :: Int -> Config -> Config
    setMaxJobs n c = c {configMaxParallelJobs = Just n}



    setPrintCpuTime x = x >>= \c -> return c {configPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {configDryRun = True}
    setFastFail     x = x >>= \c -> return c {configFastFail = True}
    setRerun        x = x >>= \c -> return c {configRerun = True}
    setNoColor      x = x >>= \c -> return c {configColorMode = ColorNever}
    setColor        x = x >>= \c -> return c {configColorMode = ColorAlways}
    clearMaxJobs    x = x >>= \c -> return c {configMaxParallelJobs = Nothing}

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

parseOptions :: Config -> String -> [String] -> Either (ExitCode, String) Config
parseOptions c prog args = case getOpt Permute (options ++ undocumentedOptions) args of
    (opts, [], []) -> case foldl' (flip id) (Right c) opts of
        Left Help                         -> Left (ExitSuccess, usageInfo ("Usage: " ++ prog ++ " [OPTION]...\n\nOPTIONS") options)
        Left (InvalidArgument flag value) -> tryHelp ("invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'\n")
        Right x -> Right x
    (_, _, err:_)  -> tryHelp err
    (_, arg:_, _)  -> tryHelp ("unexpected argument `" ++ arg ++ "'\n")
  where
    tryHelp msg = Left (ExitFailure 1, prog ++ ": " ++ msg ++ "Try `" ++ prog ++ " --help' for more information.\n")
