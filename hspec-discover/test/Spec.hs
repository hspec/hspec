module Main (main) where

import           Test.Hspec.Monadic
import qualified RunSpec

main :: IO ()
main = hspecX $ do
  describe "Run" RunSpec.spec
