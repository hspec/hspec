{-# LANGUAGE CPP #-}
module Test.Hspec.Core.FailureReport (
  FailureReport (..)
, writeFailureReport
, readFailureReport
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

#ifndef __GHCJS__
import           System.SetEnv
import           Test.Hspec.Core.Util (safeTry)
#endif
import           System.IO
import           System.Directory
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Config.Options (Config(..))

data FailureReport = FailureReport {
  failureReportSeed :: Integer
, failureReportMaxSuccess :: Int
, failureReportMaxSize :: Int
, failureReportMaxDiscardRatio :: Int
, failureReportPaths :: [Path]
} deriving (Eq, Show, Read)

writeFailureReport :: Config -> FailureReport -> IO ()
writeFailureReport config report = case configFailureReport config of
  Just file -> writeFile file (show report)
  Nothing -> do
#ifdef __GHCJS__
    -- ghcjs currently does not support setting environment variables
    -- (https://github.com/ghcjs/ghcjs/issues/263). Since writing a failure report
    -- into the environment is a non-essential feature we just disable this to be
    -- able to run hspec test-suites with ghcjs at all. Should be reverted once
    -- the issue is fixed.
    return ()
#else
    -- on Windows this can throw an exception when the input is too large, hence
    -- we use `safeTry` here
    safeTry (setEnv "HSPEC_FAILURES" $ show report) >>= either onError return
    where
      onError err = do
        hPutStrLn stderr ("WARNING: Could not write environment variable HSPEC_FAILURES (" ++ show err ++ ")")
#endif

readFailureReport :: Config -> IO (Maybe FailureReport)
readFailureReport config = case configFailureReport config of
  Just file -> do
    exists <- doesFileExist file
    if exists
      then do
        r <- readFile file
        let report = readMaybe r
        when (report == Nothing) $ do
          hPutStrLn stderr ("WARNING: Could not read failure report from file " ++ show file ++ "!")
        return report
      else return Nothing
  Nothing -> do
    mx <- lookupEnv "HSPEC_FAILURES"
    case mx >>= readMaybe of
      Nothing -> do
        hPutStrLn stderr "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!"
        return Nothing
      report -> return report
