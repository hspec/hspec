{-# LANGUAGE OverloadedStrings #-}
module RunSpec (spec) where

import           Test.Hspec

import           Run
import qualified Format

spec :: Spec
spec = do
  describe "dumpModuleApi" $ do
    it "dumps the API of a given module" $ do
      expected <- readFile "../../api/Test.Hspec"
      dumpModuleApi Format.format "Test.Hspec" `shouldReturn` expected
