{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Config (
  Config (..)
, defaultConfig
, getConfig
, configAddFilter
, configSetSeed
) where

import           System.IO
import           System.Exit
import           System.Environment
import qualified Test.QuickCheck as QC
import           Test.Hspec.Formatters

import           Test.Hspec.Util

-- for Monad (Either e) when base < 4.3
import           Control.Monad.Trans.Error ()

import           Test.Hspec.Options

{-# DEPRECATED configVerbose "this has no effect anymore and will be removed with a future release" #-} -- deprecated since 1.5.0
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

defaultConfig :: Config
defaultConfig = Config False False False False False Nothing QC.stdArgs ColorAuto specdoc False stdout

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
    configVerbose         = False
  , configDryRun          = optionsDryRun
  , configPrintCpuTime    = optionsPrintCpuTime
  , configRerun           = optionsRerun
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
      xs -> Just $ foldl1 (\p0 p1 path -> p0 path || p1 path) (map filterPredicate xs)

getConfig :: Options -> IO Config
getConfig c = do
  prog <- getProgName
  args <- getArgs
  case parseOptions c prog args of
    Left (err, msg) -> exitWithMessage err msg
    Right opts -> return (mkConfig opts)

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
