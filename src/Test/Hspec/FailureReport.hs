module Test.Hspec.FailureReport (
  writeFailureReport
, readFailureReport
) where

import           System.IO
import           System.SetEnv
import qualified Test.QuickCheck as QC
import           Test.Hspec.Config
import           Test.Hspec.Util (Path, safeTry, readMaybe, getEnv)

type Seed = Integer

writeFailureReport :: (Seed, [Path]) -> IO ()
writeFailureReport x = do
  -- on Windows this can throw an exception when the input is too large, hence
  -- we use `safeTry` here
  safeTry (setEnv "HSPEC_FAILURES" $ show x) >>= either onError return
  where
    onError err = do
      hPutStrLn stderr ("WARNING: Could not write environment variable HSPEC_FAILURES (" ++ show err ++ ")")

readFailureReport :: Config -> IO Config
readFailureReport c = do
  mx <- getEnv "HSPEC_FAILURES"
  case mx >>= readMaybe of
    Nothing -> do
      hPutStrLn stderr "WARNING: Could not read environment variable HSPEC_FAILURES; `--rerun' is ignored!"
      return c
    Just (seed, xs) -> do
      (return . setSeed seed . configAddFilter (`elem` xs)) c

setSeed :: Seed -> Config -> Config
setSeed seed c
  | hasSeed = c
  | otherwise = configSetSeed seed c
  where
    hasSeed = maybe False (const True) (QC.replay $ configQuickCheckArgs c)
