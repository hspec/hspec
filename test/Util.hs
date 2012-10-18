module Util where

import           Control.Applicative
import           Test.Hspec.Core
import           Test.Hspec.Runner (defaultConfig)
import qualified System.IO.Silently as S

capture :: IO a -> IO [String]
capture action = lines . fst <$> S.capture action

runSpec :: [SpecTree] -> IO [String]
runSpec s = capture (hspecWith defaultConfig s)

hspecSummary :: [SpecTree] -> IO Summary
hspecSummary = hspecWith defaultConfig
