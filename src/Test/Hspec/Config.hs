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

data Config = Config {
  configVerbose        :: Bool
, configQuickCheckArgs :: QC.Args
, configColorMode      :: ColorMode
, configFormatter      :: Formatter
, configHandle         :: Handle
}

data ColorMode = ColorAuto | ColorNever | ColorAlway

defaultConfig :: Config
defaultConfig = Config False QC.stdArgs ColorAuto specdoc stdout

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

options :: [OptDescr (Result -> Result)]
options = [
    Option []  ["help"]                    (NoArg (const $ Left Help))        "display this help and exit"
  , Option "v" ["verbose"]                 (NoArg setVerbose)                 "do not suppress output to stdout when evaluating examples"
  , Option []  ["color"]                   (OptArg setColor "WHEN")           "colorize the output.  WHEN defaults to `always' or can be `never' or `auto'."
  , Option "f" ["format"]                  (ReqArg setFormatter "FORMATTER")  formatHelp
  , Option "a" ["maximum-generated-tests"] (ReqArg setQC_MaxSuccess "NUMBER") "how many automated tests something like QuickCheck should try, by default"
  ]
  where
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
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitFailure

    tryHelp message = do
      name <- getProgName
      printAndExit $ name ++ ": " ++ message ++ "Try `" ++ name ++ " --help' for more information.\n"
