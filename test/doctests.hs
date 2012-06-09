module Main where

import           Test.DocTest

main :: IO ()
main = doctest ["Test/Hspec/Internal.hs"]
