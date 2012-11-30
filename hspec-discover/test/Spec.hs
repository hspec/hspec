{-# LINE 1 "hspec-discover/test/Spec.hs" #-}module Main where
import Test.Hspec
import qualified RunSpec
main :: IO ()
main = hspec $ describe "Run" RunSpec.spec
