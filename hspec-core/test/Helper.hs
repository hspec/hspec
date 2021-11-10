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

, withEnvironment
, inTempDirectory

, shouldUseArgs

, removeLocations
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Char
import           System.Environment (withArgs, getEnvironment)
import           System.Exit
import qualified Control.Exception as E
import           Control.Exception
import           System.IO.Silently
import           System.SetEnv
import           System.Directory
import           System.IO.Temp

import           Test.Hspec.Meta hiding (hspec, hspecResult, pending, pendingWith)
import           Test.QuickCheck hiding (Result(..))
import qualified Test.HUnit.Lang as HUnit

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.QuickCheckUtil (mkGen)
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example (Result(..), ResultStatus(..), FailureReason(..))
import           Test.Hspec.Core.Util
import qualified Test.Hspec.Core.Format as Format

import           Data.Orphans()

exceptionEq :: E.SomeException -> E.SomeException -> Bool
exceptionEq a b
  | Just ea <- E.fromException a, Just eb <- E.fromException b = ea == (eb :: E.ErrorCall)
  | Just ea <- E.fromException a, Just eb <- E.fromException b = ea == (eb :: E.ArithException)
  | otherwise = throw (HUnit.HUnitFailure Nothing $ HUnit.ExpectedButGot Nothing (formatException b) (formatException a))

deriving instance Eq FailureReason
deriving instance Eq ResultStatus
deriving instance Eq Result

deriving instance Eq Format.Result
deriving instance Eq Format.Item

instance Eq SomeException where
  (==) = exceptionEq

throwException :: IO a
throwException = E.throwIO DivideByZero

throwException_ :: IO ()
throwException_ = throwException

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` \e -> let _ = e :: ExitCode in return ()

ignoreUserInterrupt :: IO () -> IO ()
ignoreUserInterrupt action = E.catchJust (guard . (== E.UserInterrupt)) action return

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
noOpProgressCallback _ = return ()

shouldUseArgs :: HasCallStack => [String] -> (Args -> Bool) -> Expectation
shouldUseArgs args p = do
  spy <- newIORef (H.paramsQuickCheckArgs defaultParams)
  let interceptArgs item = item {H.itemExample = \params action progressCallback -> writeIORef spy (H.paramsQuickCheckArgs params) >> H.itemExample item params action progressCallback}
      spec = H.mapSpecItem_ interceptArgs $
        H.it "foo" False
  (silence . ignoreExitCode . withArgs args . H.hspec) spec
  readIORef spy >>= (`shouldSatisfy` p)

removeLocations :: H.SpecWith a -> H.SpecWith a
removeLocations = H.mapSpecItem_ (\item -> item{H.itemLocation = Nothing})

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

inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "mockery" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action
