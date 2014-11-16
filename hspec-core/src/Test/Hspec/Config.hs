module Test.Hspec.Config (
  Config (..)
, ColorMode(..)
, defaultConfig
, getConfig
, configAddFilter
, configQuickCheckArgs
) where

import           Control.Applicative
import           System.IO
import           System.Exit
import qualified Test.QuickCheck as QC

import           Test.Hspec.Core.Util
import           Test.Hspec.Options
import           Test.Hspec.FailureReport
import           Test.Hspec.Core.QuickCheckUtil (mkGen)
import           Test.Hspec.Core.Example (Params(..), defaultParams)

-- | Add a filter predicate to config.  If there is already a filter predicate,
-- then combine them with `||`.
configAddFilter :: (Path -> Bool) -> Config -> Config
configAddFilter p1 c = c {
    configFilterPredicate = Just p1 `filterOr` configFilterPredicate c
  }

mkConfig :: Maybe FailureReport -> Config -> Config
mkConfig mFailureReport opts = opts {
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

    rerunFilter = flip elem . failureReportPaths <$> mFailureReport

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

getConfig :: Config -> String -> [String] -> IO Config
getConfig opts_ prog args = do
  case parseOptions opts_ prog args of
    Left (err, msg) -> exitWithMessage err msg
    Right opts -> do
      r <- if configRerun opts then readFailureReport else return Nothing
      return (mkConfig r opts)

exitWithMessage :: ExitCode -> String -> IO a
exitWithMessage err msg = do
  hPutStr h msg
  exitWith err
  where
    h = case err of
      ExitSuccess -> stdout
      _           -> stderr
