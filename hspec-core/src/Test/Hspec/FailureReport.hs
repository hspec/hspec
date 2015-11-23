{-# LANGUAGE CPP #-}
module Test.Hspec.FailureReport (
  FailureReport (..)
, writeFailureReport
, readFailureReport
) where

#ifndef __GHCJS__
import           System.SetEnv
import           Test.Hspec.Core.Util (safeTry)
#endif
import           System.IO
import           Test.Hspec.Compat
import           Test.Hspec.Core.Util (Path)

data FailureReport = FailureReport {
  failureReportSeed :: Integer
, failureReportMaxSuccess :: Int
, failureReportMaxSize :: Int
, failureReportMaxDiscardRatio :: Int
, failureReportPaths :: [Path]
} deriving (Eq, Show, Read)

writeFailureReport :: FailureReport -> IO ()
#ifdef __GHCJS__
writeFailureReport _ = return ()
  -- ghcjs currently does not support setting environment variables
  -- (https://github.com/ghcjs/ghcjs/issues/263). Since writing a failure report
  -- into the environment is a non-essential feature we just disable this to be
  -- able to run hspec test-suites with ghcjs at all. Should be reverted once
  -- the issue is fixed.
#else
writeFailureReport x = do
  -- on Windows this can throw an exception when the input is too large, hence
  -- we use `safeTry` here
  safeTry (setEnv "HSPEC_FAILURES" $ show x) >>= either onError return
  where
    onError err = do
      hPutStrLn stderr ("WARNING: Could not write environment variable HSPEC_FAILURES (" ++ show err ++ ")")
#endif

readFailureReport :: IO (Maybe FailureReport)
readFailureReport = do
  mx <- lookupEnv "HSPEC_FAILURES"
  case mx >>= readMaybe of
    Nothing -> do
      hPutStrLn stderr "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!"
      return Nothing
    x -> return x
