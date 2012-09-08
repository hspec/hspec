module Util where

import           Test.Hspec.Core
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import           System.IO.Silently

runSpec :: Specs -> IO [String]
runSpec s = (lines . fst) `fmap` capture (hspecWith defaultConfig s)

hspecSummary :: Specs -> IO Summary
hspecSummary = hspecWith defaultConfig
