module Main where

import           Test.Hspec.Meta
import           System.SetEnv
import qualified All

spec :: Spec
spec = beforeAll_ (setEnv "IGNORE_DOT_HSPEC" "yes") $ afterAll_ (unsetEnv "IGNORE_DOT_HSPEC") All.spec

main :: IO ()
main = hspec spec
