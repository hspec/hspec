module Helper (
  module Test.Hspec.Meta
, module Test.Hspec.Compat
, module Test.QuickCheck
, module System.IO.Silently
, sleep
, timeout
, defaultParams
, noOpProgressCallback
, captureLines
, normalizeSummary

, ignoreExitCode
, ignoreUserInterrupt
, throwException

, shouldStartWith
, shouldEndWith

, shouldUseArgs
) where

import           Prelude ()
import           Test.Hspec.Compat

import           Data.List
import           Data.Char
import           Control.Monad
import           System.Environment (withArgs)
import           System.Exit
import           Control.Concurrent
import qualified Control.Exception as E
import qualified System.Timeout as System
import           Data.Time.Clock.POSIX
import           System.IO.Silently

import           Test.Hspec.Meta
import           Test.QuickCheck hiding (Result(..))

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.QuickCheckUtil (mkGen)

throwException :: IO ()
throwException = E.throwIO (E.ErrorCall "foobar")

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` \e -> let _ = e :: ExitCode in return ()

ignoreUserInterrupt :: IO () -> IO ()
ignoreUserInterrupt action = E.catchJust (guard . (== E.UserInterrupt)) action return

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

shouldStartWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldStartWith` y = x `shouldSatisfy` isPrefixOf y

shouldEndWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldEndWith` y = x `shouldSatisfy` isSuffixOf y

-- replace times in summary with zeroes
normalizeSummary :: [String] -> [String]
normalizeSummary = map f
  where
    f x | "Finished in " `isPrefixOf` x = map g x
        | otherwise = x
    g x | isNumber x = '0'
        | otherwise  = x

defaultParams :: H.Params
defaultParams = H.defaultParams {H.paramsQuickCheckArgs = stdArgs {replay = Just (mkGen 23, 0), maxSuccess = 1000}}

noOpProgressCallback :: H.ProgressCallback
noOpProgressCallback _ = return ()

sleep :: POSIXTime -> IO ()
sleep = threadDelay . floor . (* 1000000)

timeout :: POSIXTime -> IO a -> IO (Maybe a)
timeout = System.timeout . floor . (* 1000000)

shouldUseArgs :: [String] -> (Args -> Bool) -> Expectation
shouldUseArgs args p = do
  spy <- newIORef (H.paramsQuickCheckArgs defaultParams)
  let interceptArgs item = item {H.itemExample = \params action progressCallback -> writeIORef spy (H.paramsQuickCheckArgs params) >> H.itemExample item params action progressCallback}
      spec = H.mapSpecItem_ interceptArgs $
        H.it "foo" False
  (silence . ignoreExitCode . withArgs args . H.hspec) spec
  readIORef spy >>= (`shouldSatisfy` p)
