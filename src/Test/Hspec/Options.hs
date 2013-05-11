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
, optionsColorMode    :: ColorMode
, optionsFormatter    :: Formatter
, optionsHtmlOutput   :: Bool
}

addMatch :: String -> Options -> Options
addMatch s c = c {optionsMatch = s : optionsMatch c}

setMaxSuccess :: Int -> Options -> Options
setMaxSuccess n c = c {optionsMaxSuccess = Just n}

setSeed :: Integer -> Options -> Options
setSeed n c = c {optionsSeed = Just n}

data ColorMode = ColorAuto | ColorNever | ColorAlway

defaultOptions :: Options
defaultOptions = Options False False False False [] Nothing Nothing ColorAuto specdoc False

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

mkOption :: [Char] -> [Char] -> Arg a -> String -> OptDescr (Result -> Result)
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
  , mkOption "m"  "match"             (Arg "PATTERN" return addMatch)    (h "only run examples that match given PATTERN")
  , Option   []  ["color"]            (OptArg setColor "WHEN")            (h "colorize the output; WHEN defaults to `always' or can be `never' or `auto'")
  , mkOption "f"  "format"            (Arg "FORMATTER" readFormatter setFormatter) formatHelp
  , mkOption "a"  "qc-max-success"    (Arg "N" readMaybe setMaxSuccess)   (h "maximum number of successful tests before a QuickCheck property succeeds")
  , mkOption []   "seed"              (Arg "N" readMaybe setSeed)   (h "used seed for QuickCheck properties")
  , Option   []  ["print-cpu-time"]   (NoArg setPrintCpuTime)             (h "include used CPU time in summary")
  , Option   []  ["dry-run"]          (NoArg setDryRun)                   (h "pretend that everything passed; don't verify anything")
  , Option   []  ["fail-fast"]        (NoArg setFastFail)                 (h "abort on first failure")
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "only rerun examples that previously failed")
  ]
  where
    h = unlines . addLineBreaks

    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    setFormatter :: Formatter -> Options -> Options
    setFormatter f c = c {optionsFormatter = f}

    setPrintCpuTime x = x >>= \c -> return c {optionsPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {optionsDryRun       = True}
    setFastFail     x = x >>= \c -> return c {optionsFastFail     = True}
    setRerun        x = x >>= \c -> return c {optionsRerun = True}

    setColor mValue x = x >>= \c -> parseColor mValue >>= \v -> return c {optionsColorMode = v}
      where
        parseColor s = case s of
          Nothing       -> return ColorAlway
          Just "auto"   -> return ColorAuto
          Just "never"  -> return ColorNever
          Just "always" -> return ColorAlway
          Just v        -> Left (InvalidArgument "color" v)

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
