module Test.Hspec.Config (
  Config (..)
, ColorMode (..)
, defaultConfig
, getConfig
, configAddFilter
, configSetSeed

-- exported to silence warnings
, Arg (..)
) where

import           System.IO
import           System.Exit
import           System.Environment
import           System.Console.GetOpt
import qualified Test.QuickCheck as QC
import           Test.Hspec.Formatters

import           Test.Hspec.Util

-- for Monad (Either e) when base < 4.3
import           Control.Monad.Trans.Error ()

data Config = Config {
  configVerbose         :: Bool
, configDryRun          :: Bool
, configPrintCpuTime    :: Bool
, configRerun           :: Bool
, configFastFail        :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configFilterPredicate :: Maybe (Path -> Bool)
, configQuickCheckArgs  :: QC.Args
, configColorMode       :: ColorMode
, configFormatter       :: Formatter
, configHtmlOutput      :: Bool
, configHandle          :: Handle
}

{-# DEPRECATED configVerbose "this has no effect anymore and will be removed with a future release" #-} -- deprecated since 1.5.0

data ColorMode = ColorAuto | ColorNever | ColorAlway

defaultConfig :: Config
defaultConfig = Config False False False False False Nothing QC.stdArgs ColorAuto specdoc False stdout

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

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {configFilterPredicate = Just p}
  where
    -- if there is already a predicate, we combine them with ||
    p  = maybe p1 (\p0 path -> p0 path || p1 path) mp
    mp = configFilterPredicate c

setMaxSuccess :: Int -> Config -> Config
setMaxSuccess n c = c {configQuickCheckArgs = (configQuickCheckArgs c) {QC.maxSuccess = n}}

configSetSeed :: Integer -> Config -> Config
configSetSeed n c = c {configQuickCheckArgs = (configQuickCheckArgs c) {QC.replay = Just (stdGenFromInteger n, 0)}}


data Arg a = Arg {
  argumentName   :: String
, argumentParser :: String -> Maybe a
, argumentSetter :: a -> Config -> Config
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
  , mkOption "m"  "match"             (Arg "PATTERN" return setFilter)    (h "only run examples that match given PATTERN")
  , Option   []  ["color"]            (OptArg setColor "WHEN")            (h "colorize the output; WHEN defaults to `always' or can be `never' or `auto'")
  , mkOption "f"  "format"            (Arg "FORMATTER" readFormatter setFormatter) formatHelp
  , mkOption "a"  "qc-max-success"    (Arg "N" readMaybe setMaxSuccess)   (h "maximum number of successful tests before a QuickCheck property succeeds")
  , mkOption []   "seed"              (Arg "N" readMaybe configSetSeed)   (h "used seed for QuickCheck properties")
  , Option   []  ["print-cpu-time"]   (NoArg setPrintCpuTime)             (h "include used CPU time in summary")
  , Option   []  ["dry-run"]          (NoArg setDryRun)                   (h "pretend that everything passed; don't verify anything")
  , Option   []  ["fail-fast"]        (NoArg setFastFail)                 (h "abort on first failure")
  , Option   "r" ["rerun"]            (NoArg  setRerun)                   (h "only rerun examples that previously failed")
  ]
  where
    h = unlines . addLineBreaks

    setFilter :: String -> Config -> Config
    setFilter = configAddFilter . filterPredicate

    readFormatter :: String -> Maybe Formatter
    readFormatter = (`lookup` formatters)

    setFormatter :: Formatter -> Config -> Config
    setFormatter f c = c {configFormatter = f}

    setPrintCpuTime x = x >>= \c -> return c {configPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {configDryRun       = True}
    setFastFail     x = x >>= \c -> return c {configFastFail     = True}
    setRerun        x = x >>= \c -> return c {configRerun = True}

    setColor mValue x = x >>= \c -> parseColor mValue >>= \v -> return c {configColorMode = v}
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
    setHtml x = x >>= \c -> return c {configHtmlOutput = True}

parseConfig :: Config -> String -> [String] -> Either (ExitCode, String) Config
parseConfig c prog args = case getOpt Permute (options ++ undocumentedOptions) args of
    (opts, [], []) -> case foldl (flip id) (Right c) opts of
        Left Help                         -> Left (ExitSuccess, usageInfo ("Usage: " ++ prog ++ " [OPTION]...\n\nOPTIONS") options)
        Left (InvalidArgument flag value) -> tryHelp ("invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'\n")
        Right x -> Right x
    (_, _, err:_)  -> tryHelp err
    (_, arg:_, _)  -> tryHelp ("unexpected argument `" ++ arg ++ "'\n")
  where
    tryHelp msg = Left (ExitFailure 1, prog ++ ": " ++ msg ++ "Try `" ++ prog ++ " --help' for more information.\n")

getConfig :: Config -> IO Config
getConfig c = do
  prog <- getProgName
  args <- getArgs
  case parseConfig c prog args of
    Left (err, msg) -> exitWithMessage err msg
    Right config -> return config

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
