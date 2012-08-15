module Test.Hspec.Config (
  Config (..)
, defaultConfig
, getConfig
) where

import           Control.Monad (when)
import           Control.Applicative
import           System.Exit
import           System.Environment
import           System.Console.GetOpt
import qualified Test.QuickCheck as QC

data Config = Config {
  configQuickCheckArgs :: QC.Args
}

defaultConfig :: Config
defaultConfig = Config QC.stdArgs

options :: [OptDescr (Maybe Config -> Maybe Config)]
options = [
    Option []  ["help"]                    (NoArg (const Nothing))            "display this help and exit"
  , Option "a" ["maximum-generated-tests"] (ReqArg setQC_MaxSuccess "NUMBER") "how many automated tests something like QuickCheck should try, by default"
  ]
  where
    setQC_MaxSuccess :: String -> Maybe Config -> Maybe Config
    setQC_MaxSuccess _ Nothing  = Nothing
    setQC_MaxSuccess n (Just c) = Just $ c {configQuickCheckArgs = (configQuickCheckArgs c) {QC.maxSuccess = read n}}

getConfig :: IO Config
getConfig = do
  (opts, files, errors) <- getOpt Permute options <$> getArgs

  when ((not . null) errors)
    (tryHelp $ head errors)

  when ((not . null) files)
    (tryHelp $ "unrecognized option `" ++ head files ++ "'\n")

  case foldl (flip id) (Just defaultConfig) opts of
    Nothing -> do
      name <- getProgName
      putStr $ usageInfo ("Usage: " ++ name ++ " [OPTION]...\n") options
      exitSuccess
    Just config -> do
      return config
  where
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitFailure

    tryHelp message = printAndExit $ "pwsafe: " ++ message
      ++ "Try `pwsafe --help' for more information.\n"
