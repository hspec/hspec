{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Config (
  Config (..)
, ColorMode(..)
, UnicodeMode(..)
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

import           GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import           System.IO
import           System.IO.Error
import           System.Exit
import           System.FilePath
import           System.Directory
import           System.Environment (getProgName, getEnvironment)
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Config.Options
import           Test.Hspec.Core.Config.Definition
import           Test.Hspec.Core.FailureReport
import           Test.Hspec.Core.QuickCheck.Util (mkGen)
import           Test.Hspec.Core.Example (Params(..), defaultParams)
import qualified Test.Hspec.Core.Formatters.V2 as V2

defaultConfig :: Config
defaultConfig = mkDefaultConfig $ map (fmap V2.formatterToFormat) [
    ("checks", V2.checks)
  , ("specdoc", V2.specdoc)
  , ("progress", V2.progress)
  , ("failed-examples", V2.failed_examples)
  , ("silent", V2.silent)
  ]

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {
    configFilterPredicate = Just p1 `filterOr` configFilterPredicate c
  }

applyFailureReport :: Maybe FailureReport -> Config -> Config
applyFailureReport mFailureReport config = config {
    configFilterPredicate = matchFilter `filterOr` rerunFilter
  , configSeed = mSeed
  , configQuickCheckMaxSuccess = mMaxSuccess
  , configQuickCheckMaxDiscardRatio = mMaxDiscardRatio
  , configQuickCheckMaxSize = mMaxSize
  }
  where

    mSeed = configSeed config <|> deprecatedQuickCheckSeed config <|> (failureReportSeed <$> mFailureReport)
    mMaxSuccess = configQuickCheckMaxSuccess config <|> (failureReportMaxSuccess <$> mFailureReport)
    mMaxSize = configQuickCheckMaxSize config <|> (failureReportMaxSize <$> mFailureReport)
    mMaxDiscardRatio = configQuickCheckMaxDiscardRatio config <|> (failureReportMaxDiscardRatio <$> mFailureReport)

    matchFilter = configFilterPredicate config

    rerunFilter = case failureReportPaths <$> mFailureReport of
      Just [] -> Nothing
      Just xs -> Just (`elem` xs)
      Nothing -> Nothing

configQuickCheckArgs :: Config -> QC.Args
configQuickCheckArgs c = qcArgs
  where
    qcArgs = (
        maybe id setSeed (configSeed c)
      . maybe id setMaxShrinks (configQuickCheckMaxShrinks c)
      . maybe id setMaxSize (configQuickCheckMaxSize c)
      . maybe id setMaxDiscardRatio (configQuickCheckMaxDiscardRatio c)
      . maybe id setMaxSuccess (configQuickCheckMaxSuccess c)) (paramsQuickCheckArgs defaultParams)

    setMaxSuccess :: Int -> QC.Args -> QC.Args
    setMaxSuccess n args = args {QC.maxSuccess = n}

    setMaxDiscardRatio :: Int -> QC.Args -> QC.Args
    setMaxDiscardRatio n args = args {QC.maxDiscardRatio = n}

    setMaxSize :: Int -> QC.Args -> QC.Args
    setMaxSize n args = args {QC.maxSize = n}

    setMaxShrinks :: Int -> QC.Args -> QC.Args
    setMaxShrinks n args = args {QC.maxShrinks = n}

    setSeed :: Integer -> QC.Args -> QC.Args
    setSeed n args = args {QC.replay = Just (mkGen (fromIntegral n), 0)}

-- |
-- `readConfig` parses config options from several sources and constructs a
-- `Config` value.  It takes options from:
--
-- 1. @~/.hspec@ (a config file in the user's home directory)
-- 1. @.hspec@ (a config file in the current working directory)
-- 1. [environment variables starting with @HSPEC_@](https://hspec.github.io/options.html#specifying-options-through-environment-variables)
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
  env <- getEnvironment
  let envVar = words <$> lookup envVarName env
  case parseOptions opts_ prog configFiles envVar env args of
    Left (err, msg) -> exitWithMessage err msg
    Right (warnings, opts) -> do
      mapM_ (hPutStrLn stderr) warnings
      return opts

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
  mHome <- tryJust (guard . isPotentialHomeDirError) getHomeDirectory
  case mHome of
    Left _ -> return Nothing
    Right home -> readConfigFile (home </> ".hspec")
  where
    isPotentialHomeDirError e =
      isDoesNotExistError e || ioeGetErrorType e == UnsupportedOperation

readLocalConfigFile :: IO (Maybe ConfigFile)
readLocalConfigFile = do
  mName <- tryJust (guard . isDoesNotExistError) (canonicalizePath ".hspec")
  case mName of
    Left _ -> return Nothing
    Right name -> readConfigFile name

readConfigFile :: FilePath -> IO (Maybe ConfigFile)
readConfigFile name = do
  exists <- doesFileExist name
  if exists then Just . (,) name . unescapeArgs <$> readFile name else return Nothing

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
