module Util where

import           Test.Hspec
import           Test.Hspec.HUnit ()
import           System.IO.Silently
import           System.IO

runSpec :: Specs -> IO [String]
runSpec s = (lines . fst) `fmap` capture (hHspec stdout s)

hspecSummary :: Specs -> IO Summary
hspecSummary = hHspec stdout
