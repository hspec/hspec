module TestUtil where

import qualified Test.Hspec as H
import           Test.Hspec.HUnit ()
import           System.IO.Silently
import           System.IO

runSpec :: H.Specs -> IO [String]
runSpec s = (lines . fst) `fmap` capture (H.hHspec stdout s)
