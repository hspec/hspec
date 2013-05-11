{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Config (
  Config (..)
, defaultConfig
, getConfig
, configAddFilter
, configSetSeed
) where

import           Data.List
import           System.IO
import           System.Exit
import qualified Test.QuickCheck as QC
import           Test.Hspec.Formatters

import           Test.Hspec.Util

-- for Monad (Either e) when base < 4.3
import           Control.Monad.Trans.Error ()

import           Test.Hspec.Options
import           Test.Hspec.FailureReport

data Config = Config {
  configDryRun          :: Bool
, configPrintCpuTime    :: Bool
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

defaultConfig :: Config
defaultConfig = Config False False False Nothing QC.stdArgs ColorAuto specdoc False stdout

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {configFilterPredicate = Just p}
  where
    -- if there is already a predicate, we combine them with ||
    p  = maybe p1 (\p0 path -> p0 path || p1 path) mp
    mp = configFilterPredicate c

configSetSeed :: Integer -> Config -> Config
configSetSeed n c = c {configQuickCheckArgs = (configQuickCheckArgs c) {QC.replay = Just (stdGenFromInteger n, 0)}}

mkConfig :: Options -> Config
mkConfig Options {..} = Config {
    configDryRun          = optionsDryRun
  , configPrintCpuTime    = optionsPrintCpuTime
  , configFastFail        = optionsFastFail
  , configFilterPredicate = p
  , configQuickCheckArgs  = qcArgs
  , configColorMode       = optionsColorMode
  , configFormatter       = optionsFormatter
  , configHtmlOutput      = optionsHtmlOutput
  , configHandle          = stdout
  }
  where
    qcArgs = maybe id setSeed optionsSeed args
      where
        args = maybe id setMaxSuccess optionsMaxSuccess QC.stdArgs

    setMaxSuccess :: Int -> QC.Args -> QC.Args
    setMaxSuccess n args = args {QC.maxSuccess = n}

    setSeed :: Integer -> QC.Args -> QC.Args
    setSeed n args = args {QC.replay = Just (stdGenFromInteger n, 0)}

    p = case optionsMatch of
      [] -> Nothing
      xs -> Just $ foldl1' (\p0 p1 path -> p0 path || p1 path) (map filterPredicate xs)

getConfig :: Options -> String -> [String] -> IO Config
getConfig opts_ prog args = do
  case parseOptions opts_ prog args of
    Left (err, msg) -> exitWithMessage err msg
    Right opts -> handleRerun (optionsRerun opts) (mkConfig opts)

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr

handleRerun :: Bool -> Config -> IO Config
handleRerun rerun c
  | rerun = do
      r <- readFailureReport
      case r of
        Nothing -> return c
        Just (seed, paths) -> (return . setSeedIfMissing seed . configAddFilter (`elem` paths)) c
  | otherwise = return c

setSeedIfMissing :: Seed -> Config -> Config
setSeedIfMissing seed c
  | hasSeed = c
  | otherwise = configSetSeed seed c
  where
    hasSeed = maybe False (const True) (QC.replay $ configQuickCheckArgs c)
