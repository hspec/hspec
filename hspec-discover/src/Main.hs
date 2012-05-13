module Main (main) where

import           System.Environment

import           Run (run)

main :: IO ()
main = getArgs >>= run
