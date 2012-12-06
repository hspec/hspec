module Main where

import           Test.DocTest

main :: IO ()
main = doctest ["-isrc", "-optP-include", "-optPdist/build/autogen/cabal_macros.h", "src/Test/Hspec/Util.hs", "src/Test/Hspec/Formatters.hs"]
