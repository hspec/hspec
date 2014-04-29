module Test.Hspec.Config (
  Config (..)
, defaultConfig
, getConfig
, configAddFilter
, configQuickCheckArgs
) where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           System.IO
import           System.Exit
import qualified Test.QuickCheck as QC
import           Test.Hspec.Formatters

import           Test.Hspec.Util
import           Test.Hspec.Options
import           Test.Hspec.FailureReport
import           Test.Hspec.Core.QuickCheckUtil (mkGen)

data Config = Config {
  configDryRun          :: Bool
, configPrintCpuTime    :: Bool
, configFastFail        :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, configFilterPredicate :: Maybe (Path -> Bool)
, configQuickCheckSeed :: Maybe Integer
, configQuickCheckMaxSuccess :: Maybe Int
, configQuickCheckMaxDiscardRatio :: Maybe Int
, configQuickCheckMaxSize :: Maybe Int
, configSmallCheckDepth :: Int
, configColorMode       :: ColorMode
, configFormatter       :: Formatter
, configHtmlOutput      :: Bool
, configHandle          :: Either Handle FilePath
}

defaultConfig :: Config
defaultConfig = Config False False False Nothing Nothing Nothing Nothing Nothing 5 ColorAuto specdoc False (Left stdout)

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {
    configFilterPredicate = Just p1 `filterOr` configFilterPredicate c
  }

filterOr :: Maybe (Path -> Bool) -> Maybe (Path -> Bool) -> Maybe (Path -> Bool)
filterOr p1_ p2_ = case (p1_, p2_) of
  (Just p1, Just p2) -> Just $ \path -> p1 path || p2 path
  _ -> p1_ <|> p2_

mkConfig :: Maybe FailureReport -> Options -> Config
mkConfig mFailureReport opts = Config {
    configDryRun          = optionsDryRun opts
  , configPrintCpuTime    = optionsPrintCpuTime opts
  , configFastFail        = optionsFastFail opts
  , configFilterPredicate = matchFilter `filterOr` rerunFilter
  , configQuickCheckSeed = mSeed
  , configQuickCheckMaxSuccess = mMaxSuccess
  , configQuickCheckMaxDiscardRatio = mMaxDiscardRatio
  , configQuickCheckMaxSize = mMaxSize
  , configSmallCheckDepth = fromMaybe (configSmallCheckDepth defaultConfig) (optionsDepth opts)
  , configColorMode       = optionsColorMode opts
  , configFormatter       = optionsFormatter opts
  , configHtmlOutput      = optionsHtmlOutput opts
  , configHandle          = maybe (configHandle defaultConfig) Right (optionsOutputFile opts)
  }
  where

    mSeed = optionsSeed opts <|> (failureReportSeed <$> mFailureReport)
    mMaxSuccess = optionsMaxSuccess opts <|> (failureReportMaxSuccess <$> mFailureReport)
    mMaxSize = optionsMaxSize opts <|> (failureReportMaxSize <$> mFailureReport)
    mMaxDiscardRatio = optionsMaxDiscardRatio opts <|> (failureReportMaxDiscardRatio <$> mFailureReport)

    matchFilter = case optionsMatch opts of
      [] -> Nothing
      xs -> Just $ foldl1' (\p0 p1 path -> p0 path || p1 path) (map filterPredicate xs)

    rerunFilter = flip elem . failureReportPaths <$> mFailureReport

configQuickCheckArgs :: Config -> QC.Args
configQuickCheckArgs c = qcArgs
  where
    qcArgs = (
        maybe id setSeed (configQuickCheckSeed c)
      . maybe id setMaxDiscardRatio (configQuickCheckMaxDiscardRatio c)
      . maybe id setMaxSize (configQuickCheckMaxSize c)
      . maybe id setMaxSuccess (configQuickCheckMaxSuccess c)) QC.stdArgs

    setMaxSuccess :: Int -> QC.Args -> QC.Args
    setMaxSuccess n args = args {QC.maxSuccess = n}

    setMaxSize :: Int -> QC.Args -> QC.Args
    setMaxSize n args = args {QC.maxSize = n}

    setMaxDiscardRatio :: Int -> QC.Args -> QC.Args
    setMaxDiscardRatio n args = args {QC.maxDiscardRatio = n}

    setSeed :: Integer -> QC.Args -> QC.Args
    setSeed n args = args {QC.replay = Just (mkGen (fromIntegral n), 0)}

getConfig :: Options -> String -> [String] -> IO Config
getConfig opts_ prog args = do
  case parseOptions opts_ prog args of
    Left (err, msg) -> exitWithMessage err msg
    Right opts -> do
      r <- if optionsRerun opts then readFailureReport else return Nothing
      return (mkConfig r opts)

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
