module Test.Hspec.Options (
  Options (..)
, ColorMode (..)
, defaultOptions
, parseOptions

-- exported to silence warnings
, Arg (..)
) where

import           Data.List
import           System.Exit
import           System.Console.GetOpt
import           Test.Hspec.Formatters

import           Test.Hspec.Util

-- for Monad (Either e) when base < 4.3
import           Control.Monad.Trans.Error ()

data Options = Options {
  optionsDryRun       :: Bool
, optionsPrintCpuTime :: Bool
, optionsRerun        :: Bool
, optionsFastFail     :: Bool
, optionsMatch        :: [String]
, optionsMaxSuccess   :: Maybe Int
, optionsSeed         :: Maybe Integer
, optionsMaxSize      :: Maybe Int
, optionsMaxDiscardRatio :: Maybe Int
, optionsColorMode    :: ColorMode
, optionsFormatter    :: Formatter
, optionsHtmlOutput   :: Bool
, optionsOutputFile   :: Maybe FilePath
}

addMatch :: String -> Options -> Options
addMatch s c = c {optionsMatch = s : optionsMatch c}

setMaxSuccess :: Int -> Options -> Options
setMaxSuccess n c = c {optionsMaxSuccess = Just n}

setMaxSize :: Int -> Options -> Options
setMaxSize n c = c {optionsMaxSize = Just n}

setMaxDiscardRatio :: Int -> Options -> Options
setMaxDiscardRatio n c = c {optionsMaxDiscardRatio = Just n}

setSeed :: Integer -> Options -> Options
setSeed n c = c {optionsSeed = Just n}

data ColorMode = ColorAuto | ColorNever | ColorAlways
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options False False False False [] Nothing Nothing Nothing Nothing ColorAuto specdoc False Nothing

formatters :: [(String, Formatter)]
formatters = [
    ("specdoc", specdoc)
  , ("progress", progress)
  , ("failed-examples", failed_examples)
  , ("silent", silent)
  ]

formatHelp :: String
formatHelp = unlines (addLineBreaks "use a custom formatter; this can be one of:" ++ map (("   " ++) . fst) formatters)

type Result = Either NoConfig Options

data NoConfig = Help | InvalidArgument String String

data Arg a = Arg {
  argumentName   :: String
, argumentParser :: String -> Maybe a
, argumentSetter :: a -> Options -> Options
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
  , Option   []  ["color"]            (NoArg setColor)                    (h "colorize the output")
  , Option   []  ["no-color"]         (NoArg setNoColor)                  (h "do not colorize the output")
  , mkOption "f"  "format"            (Arg "FORMATTER" readFormatter setFormatter) formatHelp
  , mkOption "o"  "out"               (Arg "FILE" return setOutputFile)   (h "write output to a file instead of STDOUT")
  , mkOption "a"  "qc-max-success"    (Arg "N" readMaybe setMaxSuccess)   (h "maximum number of successful tests before a QuickCheck property succeeds")
  , mkOption ""   "qc-max-size"       (Arg "N" readMaybe setMaxSize)      (h "size to use for the biggest test cases")
  , mkOption ""   "qc-max-discard"    (Arg "N" readMaybe setMaxDiscardRatio) (h "maximum number of discarded tests per successful test before giving up")
  , mkOption []   "seed"              (Arg "N" readMaybe setSeed)         (h "used seed for QuickCheck properties")
  , Option   []  ["print-cpu-time"]   (NoArg setPrintCpuTime)             (h "include used CPU time in summary")
  , Option   []  ["dry-run"]          (NoArg setDryRun)                   (h "pretend that everything passed; don't verify anything")
  , Option   []  ["fail-fast"]        (NoArg setFastFail)                 (h "abort on first failure")
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "rerun all examples that failed in the previously test run (only works in GHCi)")
  ]
  where
    h = unlines . addLineBreaks

    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    setFormatter :: Formatter -> Options -> Options
    setFormatter f c = c {optionsFormatter = f}

    setOutputFile :: String -> Options -> Options
    setOutputFile file c = c {optionsOutputFile = Just file}

    setPrintCpuTime x = x >>= \c -> return c {optionsPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {optionsDryRun       = True}
    setFastFail     x = x >>= \c -> return c {optionsFastFail     = True}
    setRerun        x = x >>= \c -> return c {optionsRerun = True}
    setNoColor      x = x >>= \c -> return c {optionsColorMode = ColorNever}
    setColor        x = x >>= \c -> return c {optionsColorMode = ColorAlways}

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
    setHtml x = x >>= \c -> return c {optionsHtmlOutput = True}

parseOptions :: Options -> String -> [String] -> Either (ExitCode, String) Options
parseOptions c prog args = case getOpt Permute (options ++ undocumentedOptions) args of
    (opts, [], []) -> case foldl' (flip id) (Right c) opts of
        Left Help                         -> Left (ExitSuccess, usageInfo ("Usage: " ++ prog ++ " [OPTION]...\n\nOPTIONS") options)
        Left (InvalidArgument flag value) -> tryHelp ("invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'\n")
        Right x -> Right x
    (_, _, err:_)  -> tryHelp err
    (_, arg:_, _)  -> tryHelp ("unexpected argument `" ++ arg ++ "'\n")
  where
    tryHelp msg = Left (ExitFailure 1, prog ++ ": " ++ msg ++ "Try `" ++ prog ++ " --help' for more information.\n")
