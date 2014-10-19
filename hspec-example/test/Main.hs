module Main where

import Test.Hspec
import qualified Spec

main :: IO ()
main = hspec Spec.spec
