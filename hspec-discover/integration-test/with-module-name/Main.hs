module Main where

import           Test.Hspec
import qualified Spec

main :: IO ()
main = do
  putStrLn "before spec"
  hspec Spec.spec
  putStrLn "after spec"
