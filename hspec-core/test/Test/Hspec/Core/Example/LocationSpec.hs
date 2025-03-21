{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -O0 #-}
module Test.Hspec.Core.Example.LocationSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Example.Location

class SomeClass a where
  someMethod :: a -> IO ()

instance SomeClass () where

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show)

spec :: Spec
spec = do
  describe "parseAssertionFailed" $ do
    context "with pre-GHC-8.* error message" $ do
      it "extracts source location" $ do
        parseAssertionFailed "Foo.hs:4:7-12: Assertion failed\n" `shouldBe` Just (Location "Foo.hs" 4 7)

  describe "extractLocation" $ do
    context "with pattern match failure in do expression" $ do
      context "in IO" $ do
        it "extracts Location" $ do
          let location = Just $ Location file (__LINE__ + 2) 13
          Left e <- try $ do
            Just n <- return Nothing
            return (n :: Int)
          extractLocation e `shouldBe` location

#if !MIN_VERSION_base(4,12,0)
      context "in Either" $ do
        it "extracts Location" $ do
          let location = Just $ Location file (__LINE__ + 4) 15
          let
            foo :: Either () ()
            foo = do
              23 <- Right (42 :: Int)
              pass
          Left e <- try (evaluate foo)
          extractLocation e `shouldBe` location
#endif

    context "with ErrorCall" $ do
      it "extracts Location" $ do
        let
          location =
            Just $ Location file (succ __LINE__) 34
        Left e <- try (evaluate (undefined :: ()))
        extractLocation e `shouldBe` location

    context "with PatternMatchFail" $ do
      context "with single-line source span" $ do
        it "extracts Location" $ do
          let
            location = Just $ Location file (__LINE__ + 1) 40
          Left e <- try (evaluate (let Just n = Nothing in (n :: Int)))
          extractLocation e `shouldBe` location

      context "with multi-line source span" $ do
        it "extracts Location" $ do
          let location = Just $ Location file (__LINE__ + 1) 36
          Left e <- try (evaluate (case Nothing of
            Just n -> n :: Int
            ))
          extractLocation e `shouldBe` location

    context "with RecConError" $ do
      it "extracts Location" $ do
        let
          location = Just $ Location file (__LINE__ + 1) 39
        Left e <- try $ evaluate (age Person {name = "foo"})
        extractLocation e `shouldBe` location

    context "with NoMethodError" $ do
      it "extracts Location" $ do
        Left e <- try $ someMethod ()
        extractLocation e `shouldBe` Just (Location file 19 10)

    context "with AssertionFailed" $ do
      it "extracts Location" $ do
        let
          location = Just $ Location file (__LINE__ + 1) 36
        Left e <- try . evaluate $ assert False ()
        extractLocation e `shouldBe` location

  describe "parseBacktraces" $ do
    it "parses Location from Backtraces" $ do
      let
        input :: String
        input = unlines [
            "Cost-centre stack backtrace:"
          , "  ..."
          , "IPE backtrace:"
          , "  ..."
          , "HasCallStack backtrace:"
          , "  foo, called at Foo.hs:23:7 in main:Foo"
          , "  bar, called at Foo.hs:42:7 in main:Foo"
          , "  baz, called at Foo.hs:65:9 in main:Foo"
          , "..."
          , "  ..."
          ]
      parseBacktraces input `shouldBe` Just Location {
        locationFile = "Foo.hs"
      , locationLine = 65
      , locationColumn = 9
      }

  describe "parseCallStack" $ do
    it "parses Location from call stack" $ do
      let input = [
              "CallStack (from HasCallStack):"
            , "  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err"
            , "  undefined, called at test/Test/Hspec.hs:13:32 in main:Test.Hspec"
            ]
      parseCallStack input `shouldBe` Just (Location ("test" </> "Test" </> "Hspec.hs") 13 32)

  describe "parseLocation" $ do
    it "parses Location" $ do
      parseLocation "test/Test/Hspec.hs:13:32" `shouldBe` Just (Location ("test" </> "Test" </> "Hspec.hs") 13 32)

  describe "parseSourceSpan" $ do
    it "parses single-line source span" $ do
      parseSourceSpan "test/Test/Hspec.hs:25:36-51:" `shouldBe` Just (Location ("test" </> "Test" </> "Hspec.hs") 25 36)

    it "parses multi-line source span" $ do
      parseSourceSpan "test/Test/Hspec.hs:(15,7)-(17,26):" `shouldBe` Just (Location ("test" </> "Test" </> "Hspec.hs") 15 7)

file :: FilePath
file = workaroundForIssue19236 __FILE__
