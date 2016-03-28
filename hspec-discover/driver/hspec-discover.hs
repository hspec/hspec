module Main (main) where

import           System.Environment

import           Test.Hspec.Discover.Run (run)

main :: IO ()
main = getArgs >>= run
