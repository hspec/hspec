module Main where

import           Prelude ()
import           Helper

import           Test.Hspec.Meta
import qualified All

spec :: Spec
spec = aroundAll_ (withEnvironment [("IGNORE_DOT_HSPEC", "yes")]) All.spec

main :: IO ()
main = hspec spec
