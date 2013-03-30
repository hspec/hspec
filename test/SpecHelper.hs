module SpecHelper where

import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H
import           Data.List
import           Data.Char
import           Test.Hspec.Meta
import           System.IO.Silently
import           Data.Time.Clock.POSIX
import           Control.Concurrent

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
sleep t = threadDelay (floor $ t * 1000000)
