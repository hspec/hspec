module Util where

import           Control.Applicative
import           Test.Hspec.Core
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import qualified System.IO.Silently as S

capture :: IO a -> IO [String]
capture action = lines . fst <$> S.capture action

runSpec :: Specs -> IO [String]
runSpec s = capture (hspecWith defaultConfig s)

hspecSummary :: Specs -> IO Summary
hspecSummary = hspecWith defaultConfig
