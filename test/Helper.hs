module Helper (
  module Test.Hspec.Meta
, module Test.QuickCheck
, module Control.Applicative
, module System.IO.Silently
, sleep
, timeout
, defaultParams
, captureLines
, normalizeSummary

, ignoreExitCode
, ignoreUserInterrupt

, shouldStartWith
, shouldEndWith

, shouldUseArgs
) where

import           Data.List
import           Data.Char
import           Data.IORef
import           Control.Monad
import           Control.Applicative
import           System.Environment (withArgs)
import           System.Exit
import           Control.Concurrent
import qualified Control.Exception as E
import qualified System.Timeout as System
import           Data.Time.Clock.POSIX
import           System.IO.Silently

import           Test.Hspec.Meta
import           Test.QuickCheck hiding (Result(..))

import qualified Test.Hspec as H
import qualified Test.Hspec.Core as H (Params(..), Item(..), mapSpecItem)
import qualified Test.Hspec.Runner as H

ignoreExitCode :: IO () -> IO ()
ignoreExitCode action = action `E.catch` \e -> let _ = e :: ExitCode in return ()

ignoreUserInterrupt :: IO () -> IO ()
ignoreUserInterrupt action = action `E.catch` \e -> unless (e == E.UserInterrupt) (E.throwIO e)

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

shouldStartWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldStartWith` y = x `shouldSatisfy` isPrefixOf y

shouldEndWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldEndWith` y = x `shouldSatisfy` isSuffixOf y

-- replace times in summary with zeroes
normalizeSummary :: [String] -> [String]
normalizeSummary xs = map f xs
  where
    f x | "Finished in " `isPrefixOf` x = map g x
        | otherwise = x
    g x | isNumber x = '0'
        | otherwise  = x

defaultParams :: H.Params
defaultParams = H.Params (H.configQuickCheckArgs H.defaultConfig) (H.configSmallCheckDepth H.defaultConfig) (const $ return ())

sleep :: POSIXTime -> IO ()
sleep = threadDelay . floor . (* 1000000)

timeout :: POSIXTime -> IO a -> IO (Maybe a)
timeout = System.timeout . floor . (* 1000000)

shouldUseArgs :: [String] -> (Args -> Bool) -> Expectation
shouldUseArgs args p = do
  spy <- newIORef (H.paramsQuickCheckArgs defaultParams)
  let interceptArgs item = item {H.itemExample = \params action -> writeIORef spy (H.paramsQuickCheckArgs params) >> H.itemExample item params action}
      spec = H.mapSpecItem interceptArgs $
        H.it "foo" False
  (silence . ignoreExitCode . withArgs args . H.hspec) spec
  readIORef spy >>= (`shouldSatisfy` p)
