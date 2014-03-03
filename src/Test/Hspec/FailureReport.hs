module Test.Hspec.FailureReport (
  FailureReport (..)
, writeFailureReport
, readFailureReport
) where

import           System.IO
import           Text.Read.Compat
import           System.SetEnv
import           Test.Hspec.Compat
import           Test.Hspec.Util (Path, safeTry)

data FailureReport = FailureReport {
  failureReportSeed :: Integer
, failureReportMaxSuccess :: Int
, failureReportMaxSize :: Int
, failureReportMaxDiscardRatio :: Int
, failureReportPaths :: [Path]
} deriving (Eq, Show, Read)

writeFailureReport :: FailureReport -> IO ()
writeFailureReport x = do
  -- on Windows this can throw an exception when the input is too large, hence
  -- we use `safeTry` here
  safeTry (setEnv "HSPEC_FAILURES" $ show x) >>= either onError return
  where
    onError err = do
      hPutStrLn stderr ("WARNING: Could not write environment variable HSPEC_FAILURES (" ++ show err ++ ")")

readFailureReport :: IO (Maybe FailureReport)
readFailureReport = do
  mx <- lookupEnv "HSPEC_FAILURES"
  case mx >>= readMaybe of
    Nothing -> do
      hPutStrLn stderr "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!"
      return Nothing
    x -> return x
