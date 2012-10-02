module Test.Hspec.Config (
  Config (..)
, ColorMode (..)
, defaultConfig
, getConfig
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

data Config = Config {
  configVerbose         :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configFilterPredicate :: Maybe (Path -> Bool)
, configQuickCheckArgs  :: QC.Args
, configColorMode       :: ColorMode
, configFormatter       :: Formatter
, configHandle          :: Handle
}

data ColorMode = ColorAuto | ColorNever | ColorAlway

defaultConfig :: Config
defaultConfig = Config False Nothing QC.stdArgs ColorAuto specdoc stdout

formatters :: [(String, Formatter)]
formatters = [
    ("specdoc", specdoc)
  , ("progress", progress)
  , ("failed-examples", failed_examples)
  , ("silent", silent)
  ]

formatHelp :: String
formatHelp = unlines ("Use a custom formatter.  This can be one of:" : map (("  " ++) . fst) formatters)

type Result = Either NoConfig Config

data NoConfig = Help | InvalidArgument String String

-- | Add a filter predicate to config.
--
-- If there is already a filter predicate, they are combined with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {configFilterPredicate = Just p}
  where
    -- if there is already a predicate, we combine them with ||
    p  = maybe p1 (\p0 path -> p0 path || p1 path) mp
    mp = configFilterPredicate c

options :: [OptDescr (Result -> Result)]
options = [
    Option []  ["help"]                    (NoArg (const $ Left Help))        "display this help and exit"
  , Option "v" ["verbose"]                 (NoArg setVerbose)                 "do not suppress output to stdout when evaluating examples"
  , Option "m" ["match"]                   (ReqArg setFilter "PATTERN")       "only run examples that match given PATTERN"
  , Option []  ["color"]                   (OptArg setColor "WHEN")           "colorize the output.  WHEN defaults to `always' or can be `never' or `auto'."
  , Option "f" ["format"]                  (ReqArg setFormatter "FORMATTER")  formatHelp
  , Option "a" ["maximum-generated-tests"] (ReqArg setQC_MaxSuccess "NUMBER") "how many automated tests something like QuickCheck should try, by default"
  ]
  where
    setFilter :: String -> Result -> Result
    setFilter pattern x = configAddFilter (filterPredicate pattern) <$> x

    setVerbose x = x >>= \c -> return c {configVerbose = True}

    setFormatter name x = x >>= \c -> case lookup name formatters of
      Nothing -> Left (InvalidArgument "format" name)
      Just f  -> return c {configFormatter = f}

    setQC_MaxSuccess n x = x >>= \c -> return c {configQuickCheckArgs = (configQuickCheckArgs c) {QC.maxSuccess = read n}}

    setColor mValue x = x >>= \c -> parseColor mValue >>= \v -> return c {configColorMode = v}
      where
        parseColor s = case s of
          Nothing       -> return ColorAuto
          Just "auto"   -> return ColorAuto
          Just "never"  -> return ColorNever
          Just "always" -> return ColorAlway
          Just v        -> Left (InvalidArgument "color" v)

getConfig :: IO Config
getConfig = do
  (opts, args, errors) <- getOpt Permute options <$> getArgs

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
