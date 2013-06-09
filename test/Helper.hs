module Helper (
  module Test.Hspec.Meta
, module Test.QuickCheck
, sleep
, timeout
, defaultParams
, captureLines
, normalizeSummary

, shouldStartWith
, shouldEndWith
, shouldContain
) where

import           Test.Hspec.Meta
import           Test.QuickCheck hiding (Result(..))

import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H
import           Data.List
import           Data.Char
import           System.IO.Silently
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import qualified System.Timeout as System

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

shouldContain :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldContain` y = x `shouldSatisfy` isInfixOf y

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
defaultParams = H.Params (H.configQuickCheckArgs H.defaultConfig) (const $ return ())

sleep :: POSIXTime -> IO ()
sleep = threadDelay . floor . (* 1000000)

timeout :: POSIXTime -> IO a -> IO (Maybe a)
timeout = System.timeout . floor . (* 1000000)
