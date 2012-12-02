module Util where

import           Data.List
import           Data.Char
import           Test.Hspec.Meta
import           System.IO.Silently

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
