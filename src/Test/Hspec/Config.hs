module Test.Hspec.Config (
  Config (..)
, ColorMode (..)
, defaultConfig
, getConfig
, configAddFilter
) where

import           Control.Monad (unless)
import           Control.Applicative
import           System.IO
import           System.Exit
import           System.Environment
import           System.Console.GetOpt
import qualified Test.QuickCheck as QC
import           Test.Hspec.Formatters

import           Test.Hspec.Util
import           Test.Hspec.Core.Type (Params (..), defaultParams)

-- for Monad (Either e) when base < 4.3
import           Control.Monad.Trans.Error ()

data Config = Config {
  configVerbose         :: Bool
, configDryRun          :: Bool
, configPrintCpuTime    :: Bool
, configReRun           :: Bool
, configFastFail        :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configFilterPredicate :: Maybe (Path -> Bool)
, configParams          :: Params
, configColorMode       :: ColorMode
, configFormatter       :: Formatter
, configHtmlOutput      :: Bool
, configHandle          :: Handle
}

data ColorMode = ColorAuto | ColorNever | ColorAlway

defaultConfig :: Config
defaultConfig = Config False False False False False Nothing defaultParams ColorAuto specdoc False stdout

formatters :: [(String, Formatter)]
formatters = [
    ("specdoc", specdoc)
  , ("progress", progress)
  , ("failed-examples", failed_examples)
  , ("silent", silent)
  ]

formatHelp :: String
formatHelp = unlines ("Use a custom formatter.  This can be one of:" : map (("   " ++) . fst) formatters)

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

setQC_MaxSuccess :: String -> Result -> Result
setQC_MaxSuccess n x = (mapParams $ \p -> p {paramsQuickCheckArgs = (paramsQuickCheckArgs p) {QC.maxSuccess = read n}}) <$> x
  where
    mapParams :: (Params -> Params) -> Config -> Config
    mapParams f c = c {configParams = f (configParams c)}

options :: [OptDescr (Result -> Result)]
options = [
    Option []  ["help"]                    (NoArg (const $ Left Help))        "display this help and exit"
  , Option "v" ["verbose"]                 (NoArg setVerbose)                 "do not suppress output to stdout when\nevaluating examples"
  , Option "m" ["match"]                   (ReqArg setFilter "PATTERN")       "only run examples that match given PATTERN"
  , Option []  ["color"]                   (OptArg setColor "WHEN")           "colorize the output.  WHEN defaults to\n`always' or can be `never' or `auto'."
  , Option "f" ["format"]                  (ReqArg setFormatter "FORMATTER")  formatHelp
  , Option "a" ["qc-max-success"]          (ReqArg setQC_MaxSuccess "N")      "maximum number of successful tests before a\nQuickCheck property succeeds"
  , Option []  ["print-cpu-time"]          (NoArg setPrintCpuTime)            "include used CPU time in summary"
  , Option []  ["dry-run"]                 (NoArg setDryRun)                  "pretend that everything passed; don't verify\nanything"
  , Option []  ["fast-fail"]               (NoArg setFastFail)                "stop after first failure"
  ]
  where
    setFilter :: String -> Result -> Result
    setFilter pattern x = configAddFilter (filterPredicate pattern) <$> x

    setVerbose      x = x >>= \c -> return c {configVerbose      = True}
    setPrintCpuTime x = x >>= \c -> return c {configPrintCpuTime = True}
    setDryRun       x = x >>= \c -> return c {configDryRun       = True}
    setFastFail     x = x >>= \c -> return c {configFastFail     = True}

    setFormatter name x = x >>= \c -> case lookup name formatters of
      Nothing -> Left (InvalidArgument "format" name)
      Just f  -> return c {configFormatter = f}

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
    Option "r" ["re-run"]                  (NoArg  setReRun)                  "only re-run examples that previously failed"

    -- for compatibility with test-framework
  , Option ""  ["maximum-generated-tests"] (ReqArg setQC_MaxSuccess "NUMBER") "how many automated tests something like QuickCheck should try, by default"

    -- undocumented for now, as we probably want to change this to produce a
    -- standalone HTML report in the future
  , Option []  ["html"]                    (NoArg setHtml)                    "produce HTML output"
  ]
  where
    setReRun :: Result -> Result
    setReRun x = x >>= \c -> return c {configReRun = True}

    setHtml :: Result -> Result
    setHtml x = x >>= \c -> return c {configHtmlOutput = True}

getConfig :: IO Config
getConfig = do
  (opts, args, errors) <- getOpt Permute (options ++ undocumentedOptions) <$> getArgs

  unless (null errors)
    (tryHelp $ head errors)

  unless (null args)
    (tryHelp $ "unexpected argument `" ++ head args ++ "'\n")

  case foldl (flip id) (Right defaultConfig) opts of
    Left Help -> do
      name <- getProgName
      putStr $ usageInfo ("Usage: " ++ name ++ " [OPTION]...\n\nOPTIONS") options
      exitSuccess
    Left (InvalidArgument flag value) -> do
      tryHelp $ "invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'\n"
    Right config -> do
      return config
  where
    tryHelp message = do
      name <- getProgName
      hPutStr stderr $ name ++ ": " ++ message ++ "Try `" ++ name ++ " --help' for more information.\n"
      exitFailure
