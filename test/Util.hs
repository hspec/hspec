module Util where

import           Data.List
import           Test.Hspec.Meta
import qualified System.IO.Silently as S

capture_ :: IO a -> IO String
capture_ = fmap fst . S.capture

capture__ :: IO a -> IO [String]
capture__ = fmap lines . capture_

shouldContain :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldContain` y = x `shouldSatisfy` isInfixOf y

shouldStartWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldStartWith` y = x `shouldSatisfy` isPrefixOf y

shouldEndWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
x `shouldEndWith` y = x `shouldSatisfy` isSuffixOf y
