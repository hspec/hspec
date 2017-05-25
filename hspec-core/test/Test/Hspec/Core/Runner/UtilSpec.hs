{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Runner.UtilSpec (spec) where

import           Helper
import           Control.Exception

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Runner.Util

spec :: Spec
spec = do
  describe "extractLocation" $ do
    it "extracts Location from exception" $ do
      let
        location =
#if MIN_VERSION_base(4,9,0)
          Just $ Location __FILE__ (__LINE__ + 4) 32
#else
          Nothing
#endif
      Left e <- try (evaluate (undefined :: ()))
      extractLocation e `shouldBe` location

  describe "parseCallStack" $ do
    it "parses Location from call stack" $ do
      let input = unlines [
              "CallStack (from HasCallStack):"
            , "  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err"
            , "  undefined, called at test/Test/Hspec.hs:13:32 in main:Test.Hspec"
            ]
      parseCallStack input `shouldBe` Just (Location "test/Test/Hspec.hs" 13 32)

  describe "parseLocation" $ do
    it "parses Location" $ do
      parseLocation "test/Test/Hspec.hs:13:32" `shouldBe` Just (Location "test/Test/Hspec.hs" 13 32)
