{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec.Meta
, module Test.Hspec.Core.Compat
, module Test.QuickCheck
, module System.IO.Silently
, Seconds(..)
, sleep
, timeout
, defaultParams
, noOpProgressCallback
, captureLines
, normalizeSummary
, normalizeTimes

, ignoreExitCode
, ignoreUserInterrupt
, throwException
, throwException_

, withArgs
, withEnvironment
, withTempDirectory
, inTempDirectory

, hspecSilent
, hspecResultSilent
, hspecCapture
, shouldUseArgs

, removeLocations

, (</>)
, mkLocation
, workaroundForIssue19236

, replace

, red
, green
, colorize
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Char
import           System.Environment (withArgs, getEnvironment, setEnv, unsetEnv)
import           System.Exit
import           System.IO.Silently
import           System.FilePath
import           System.Directory
import           System.IO.Temp (withSystemTempDirectory)
import           System.Console.ANSI

import           Test.Hspec.Meta hiding (hspec, hspecResult, pending, pendingWith)
import           Test.QuickCheck hiding (Result(..), Discard(..))
import qualified Test.HUnit.Lang as HUnit

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.QuickCheckUtil (mkGen)
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example (Result(..), ResultStatus(..), FailureReason(..), Location(..))
import           Test.Hspec.Core.Example.Location (workaroundForIssue19236)
import           Test.Hspec.Core.Util
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Formatters.V2 (formatterToFormat, silent)

import           Data.Orphans()

exceptionEq :: SomeException -> SomeException -> Bool
exceptionEq a b
  | Just ea <- fromException a, Just eb <- fromException b = ea == (eb :: ErrorCall)
  | Just ea <- fromException a, Just eb <- fromException b = ea == (eb :: ArithException)
  | otherwise = throw (HUnit.HUnitFailure Nothing $ HUnit.ExpectedButGot Nothing (formatException b) (formatException a))

deriving instance Eq FailureReason
deriving instance Eq ResultStatus
deriving instance Eq Result

deriving instance Eq Format.Result
deriving instance Eq Format.Item

instance Eq SomeException where
  (==) = exceptionEq

throwException :: IO a
throwException = throwIO DivideByZero

throwException_ :: IO ()
throwException_ = throwException

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `catch` \e -> let _ = e :: ExitCode in pass

ignoreUserInterrupt :: IO () -> IO ()
ignoreUserInterrupt action = catchJust (guard . (== UserInterrupt)) action return

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

-- replace times in summary with zeroes
normalizeSummary :: [String] -> [String]
normalizeSummary = map f
  where
    f x | "Finished in " `isPrefixOf` x = map g x
        | otherwise = x
    g x | isNumber x = '0'
        | otherwise  = x

normalizeTimes :: [String] -> [String]
normalizeTimes = map go
  where
    go xs = case xs of
      [] -> []
      '(' : y : ys | isNumber y, Just zs <- stripPrefix "ms)" $ dropWhile isNumber ys -> "(2ms)" ++ go zs
      y : ys -> y : go ys

defaultParams :: H.Params
defaultParams = H.defaultParams {H.paramsQuickCheckArgs = stdArgs {replay = Just (mkGen 23, 0), maxSuccess = 1000}}

noOpProgressCallback :: H.ProgressCallback
noOpProgressCallback _ = pass

silentConfig :: H.Config
silentConfig = H.defaultConfig {H.configFormat = Just $ formatterToFormat silent}

hspecSilent :: H.Spec -> IO ()
hspecSilent = H.hspecWith silentConfig

hspecResultSilent :: H.Spec -> IO H.Summary
hspecResultSilent = H.hspecWithResult silentConfig

hspecCapture :: [String] -> H.Spec -> IO String
hspecCapture args = fmap (unlines . normalizeSummary) . captureLines . ignoreExitCode . withArgs args . H.hspec . removeLocations

shouldUseArgs :: HasCallStack => (Eq n, Show n) => [String] -> (Args -> n,  n) -> Expectation
shouldUseArgs args (accessor, expected) = do
  spy <- newIORef stdArgs
  let
    interceptArgs :: H.Item a -> H.Item a
    interceptArgs item = item {
      H.itemExample = \ params action progressCallback -> do
        writeIORef spy (H.paramsQuickCheckArgs params)
        H.itemExample item params action progressCallback
    }
    spec :: H.Spec
    spec = H.mapSpecItem_ interceptArgs $ H.it "" True
  withArgs args $ hspecSilent spec
  accessor <$> readIORef spy `shouldReturn` expected

removeLocations :: H.SpecWith a -> H.SpecWith a
removeLocations = H.mapSpecItem_ $ \ item -> item {
  H.itemLocation = Nothing
, H.itemExample = \ params action progressCallback -> removeResultLocation <$> H.itemExample item params action progressCallback
}

removeResultLocation :: Result -> Result
removeResultLocation (Result info status) = case status of
  Success -> Result info status
  Pending _loc reason -> Result info (Pending Nothing reason)
  Failure _loc reason -> Result info (Failure Nothing reason)

withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment environment action = bracket saveEnv restoreEnv $ const action
  where
    saveEnv :: IO [(String, String)]
    saveEnv = do
      env <- clearEnv
      forM_ environment $ uncurry setEnv
      return env
    restoreEnv :: [(String, String)] -> IO ()
    restoreEnv env = do
      _ <- clearEnv
      forM_ env $ uncurry setEnv
    clearEnv :: IO [(String, String)]
    clearEnv = do
      env <- getEnvironment
      forM_ env (unsetEnv . fst)
      return env

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory = withSystemTempDirectory "hspec"

inTempDirectory :: IO a -> IO a
inTempDirectory action = withTempDirectory $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action

mkLocation :: FilePath -> Int -> Int -> Maybe Location
mkLocation file line column = Just (Location (workaroundForIssue19236 file) line column)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y xs = case break (== x) xs of
  (ys, _: zs) -> ys ++ y : zs
  _ -> xs

green :: String -> String
green = colorize Foreground Green

red :: String -> String
red = colorize Foreground Red

colorize :: ConsoleLayer -> Color -> String -> String
colorize layer color text = setSGRCode [SetColor layer Dull color] <> text <> setSGRCode [Reset]
