module Test.Hspec.FailureReport where

import           System.IO
import           System.SetEnv
import           Test.Hspec.Config
import           Test.Hspec.Util (safeEvaluate, readMaybe, getEnv)

writeFailureReport :: String -> IO ()
writeFailureReport x = do
  -- on Windows this can throw an exception when the input is too large, hence
  -- we use `safeEvaluate` here
  r <- safeEvaluate (setEnv "HSPEC_FAILURES" x)
  either onError return r
  where
    onError err = do
      hPutStrLn stderr ("WARNING: Could not write environment variable HSPEC_FAILURES (" ++ show err ++ ")")

readFailureReport :: Config -> IO Config
readFailureReport c = do
  mx <- getEnv "HSPEC_FAILURES"
  case mx >>= readMaybe of
    Nothing -> do
      hPutStrLn stderr "WARNING: Could not read environment variable HSPEC_FAILURES; `--re-run' is ignored!"
      return c
    Just xs -> do
      return $ configAddFilter (`elem` xs) c
