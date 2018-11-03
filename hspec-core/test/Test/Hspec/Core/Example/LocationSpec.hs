{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Test.Hspec.Core.Example.LocationSpec (spec) where

import           Helper
import           Control.Exception

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Example.Location

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show)

spec :: Spec
spec = do
  describe "extractLocation" $ do
    context "with pattern match failure in do expression" $ do
      context "in IO" $ do
        it "extracts Location" $ do
          let location = Just $ Location __FILE__ (__LINE__ + 2) 13
          Left e <- try $ do
            Just n <- return Nothing
            return (n :: Int)
          extractLocation e `shouldBe` location

#if !MIN_VERSION_base(4,12,0)
      context "in Either" $ do
        it "extracts Location" $ do
          let location = Just $ Location __FILE__ (__LINE__ + 4) 15
          let
            foo :: Either () ()
            foo = do
              23 <- Right (42 :: Int)
              return ()
          Left e <- try (evaluate foo)
          extractLocation e `shouldBe` location
#endif

    context "with ErrorCall" $ do
      it "extracts Location" $ do
        let
          location =
#if MIN_VERSION_base(4,9,0)
            Just $ Location __FILE__ (__LINE__ + 4) 34
#else
            Nothing
#endif
        Left e <- try (evaluate (undefined :: ()))
        extractLocation e `shouldBe` location

    context "with PatternMatchFail" $ do
      context "with single-line source span" $ do
        it "extracts Location" $ do
          let
            location = Just $ Location __FILE__ (__LINE__ + 1) 40
          Left e <- try (evaluate (let Just n = Nothing in (n :: Int)))
          extractLocation e `shouldBe` location

      context "with multi-line source span" $ do
        it "extracts Location" $ do
          let location = Just $ Location __FILE__ (__LINE__ + 1) 36
          Left e <- try (evaluate (case Nothing of
            Just n -> n :: Int
            ))
          extractLocation e `shouldBe` location

    context "with RecConError" $ do
      it "extracts Location" $ do
        let
          location = Just $ Location __FILE__ (__LINE__ + 1) 39
        Left e <- try $ evaluate (age Person {name = "foo"})
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

  describe "parseSourceSpan" $ do
    it "parses single-line source span" $ do
      parseSourceSpan "test/Test/Hspec.hs:25:36-51:" `shouldBe` Just (Location "test/Test/Hspec.hs" 25 36)

    it "parses multi-line source span" $ do
      parseSourceSpan "test/Test/Hspec.hs:(15,7)-(17,26):" `shouldBe` Just (Location "test/Test/Hspec.hs" 15 7)
