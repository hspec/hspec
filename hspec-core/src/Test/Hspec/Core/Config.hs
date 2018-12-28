{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Config (
  Config (..)
, ColorMode(..)
, defaultConfig
, readConfig
, configAddFilter
, configQuickCheckArgs

, readFailureReportOnRerun
, applyFailureReport
#ifdef TEST
, readConfigFiles
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Data.Maybe
import           System.IO
import           System.IO.Error
import           System.Exit
import           System.FilePath
import           System.Directory
import           System.Environment (getProgName)
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Config.Options
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheckUtil (mkGen)
import           Test.Hspec.Core.Example (Params(..), defaultParams)

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {
    configFilterPredicate = Just p1 `filterOr` configFilterPredicate c
  }

applyFailureReport :: Maybe FailureReport -> Config -> Config
applyFailureReport mFailureReport opts = opts {
    configFilterPredicate = matchFilter `filterOr` rerunFilter
  , configQuickCheckSeed = mSeed
  , configQuickCheckMaxSuccess = mMaxSuccess
  , configQuickCheckMaxDiscardRatio = mMaxDiscardRatio
  , configQuickCheckMaxSize = mMaxSize
  }
  where

    mSeed = configQuickCheckSeed opts <|> (failureReportSeed <$> mFailureReport)
    mMaxSuccess = configQuickCheckMaxSuccess opts <|> (failureReportMaxSuccess <$> mFailureReport)
    mMaxSize = configQuickCheckMaxSize opts <|> (failureReportMaxSize <$> mFailureReport)
    mMaxDiscardRatio = configQuickCheckMaxDiscardRatio opts <|> (failureReportMaxDiscardRatio <$> mFailureReport)

    matchFilter = configFilterPredicate opts

    rerunFilter = case failureReportPaths <$> mFailureReport of
      Just [] -> Nothing
      Just xs -> Just (`elem` xs)
      Nothing -> Nothing

configQuickCheckArgs :: Config -> QC.Args
configQuickCheckArgs c = qcArgs
  where
    qcArgs = (
        maybe id setSeed (configQuickCheckSeed c)
      . maybe id setMaxDiscardRatio (configQuickCheckMaxDiscardRatio c)
      . maybe id setMaxSize (configQuickCheckMaxSize c)
      . maybe id setMaxSuccess (configQuickCheckMaxSuccess c)) (paramsQuickCheckArgs defaultParams)

    setMaxSuccess :: Int -> QC.Args -> QC.Args
    setMaxSuccess n args = args {QC.maxSuccess = n}

    setMaxSize :: Int -> QC.Args -> QC.Args
    setMaxSize n args = args {QC.maxSize = n}

    setMaxDiscardRatio :: Int -> QC.Args -> QC.Args
    setMaxDiscardRatio n args = args {QC.maxDiscardRatio = n}

    setSeed :: Integer -> QC.Args -> QC.Args
    setSeed n args = args {QC.replay = Just (mkGen (fromIntegral n), 0)}

-- |
-- `readConfig` parses config options from several sources and constructs a
-- `Config` value.  It takes options from:
--
-- 1. @~/.hspec@ (a config file in the user's home directory)
-- 1. @.hspec@ (a config file in the current working directory)
-- 1. the environment variable @HSPEC_OPTIONS@
-- 1. the provided list of command-line options (the second argument to @readConfig@)
--
-- (precedence from low to high)
--
-- When parsing fails then @readConfig@ writes an error message to `stderr` and
-- exits with `exitFailure`.
--
-- When @--help@ is provided as a command-line option then @readConfig@ writes
-- a help message to `stdout` and exits with `exitSuccess`.
--
-- A common way to use @readConfig@ is:
--
-- @
-- `System.Environment.getArgs` >>= readConfig `defaultConfig`
-- @
readConfig :: Config -> [String] -> IO Config
readConfig opts_ args = do
  prog <- getProgName
  configFiles <- do
    ignore <- ignoreConfigFile opts_ args
    case ignore of
      True -> return []
      False -> readConfigFiles
  envVar <- fmap words <$> lookupEnv envVarName
  case parseOptions opts_ prog configFiles envVar args of
    Left (err, msg) -> exitWithMessage err msg
    Right opts -> return opts

readFailureReportOnRerun :: Config -> IO (Maybe FailureReport)
readFailureReportOnRerun config
  | configRerun config = readFailureReport config
  | otherwise = return Nothing

readConfigFiles :: IO [ConfigFile]
readConfigFiles = do
  global <- readGlobalConfigFile
  local <- readLocalConfigFile
  return $ catMaybes [global, local]

readGlobalConfigFile :: IO (Maybe ConfigFile)
readGlobalConfigFile = do
  mHome <- tryJust (guard . isDoesNotExistError) getHomeDirectory
  case mHome of
    Left _ -> return Nothing
    Right home -> readConfigFile (home </> ".hspec")

readLocalConfigFile :: IO (Maybe ConfigFile)
readLocalConfigFile = do
  mName <- tryJust (guard . isDoesNotExistError) (canonicalizePath ".hspec")
  case mName of
    Left _ -> return Nothing
    Right name -> readConfigFile name

readConfigFile :: FilePath -> IO (Maybe ConfigFile)
readConfigFile name = do
  exists <- doesFileExist name
  if exists then Just . (,) name . words <$> readFile name else return Nothing

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
