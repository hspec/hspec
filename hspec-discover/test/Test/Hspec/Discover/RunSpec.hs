module Test.Hspec.Discover.RunSpec (spec) where

import           Helper
import           Test.Mockery.Directory

import           Test.Hspec.Discover.Run hiding (Spec)
import qualified Test.Hspec.Discover.Run as Run

spec :: Spec
spec = do
  describe "run" $ around_ inTempDirectory $ do
    it "generates a test driver" $ do
      touch "test/FooSpec.hs"
      touch "test/Foo/Bar/BazSpec.hs"
      touch "test/Foo/BarSpec.hs"
      run ["test/Spec.hs", "", "out"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Main where"
        , "import qualified FooSpec"
        , "import qualified Foo.BarSpec"
        , "import qualified Foo.Bar.BazSpec"
        , "import Test.Hspec.Discover"
        , "main :: IO ()"
        , "main = hspec spec"
        , "spec :: Spec"
        , "spec = " ++ unwords [
               "describe \"Foo\" FooSpec.spec"
          , ">> describe \"Foo.Bar\" Foo.BarSpec.spec"
          , ">> describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec"
          ]
        ]

    it "generates a test driver with wrap spec" $ do
      touch "test/SpecWrap.hs"
      touch "test/FooSpec.hs"
      touch "test/Foo/Bar/BazSpec.hs"
      touch "test/Foo/BarSpec.hs"
      run ["test/Spec.hs", "", "out"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Main where"
        , "import qualified SpecWrap"
        , "import qualified FooSpec"
        , "import qualified Foo.BarSpec"
        , "import qualified Foo.Bar.BazSpec"
        , "import Test.Hspec.Discover"
        , "main :: IO ()"
        , "main = hspec spec"
        , "spec :: Spec"
        , "spec = " ++ unwords [
               "SpecWrap.wrapSpec (describe \"Foo\" FooSpec.spec)"
          , ">> SpecWrap.wrapSpec (describe \"Foo.Bar\" Foo.BarSpec.spec)"
          , ">> SpecWrap.wrapSpec (describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec)"
          ]
        ]


    it "generates a test driver with no Main/main" $ do
      touch "test/FooSpec.hs"
      touch "test/Foo/Bar/BazSpec.hs"
      touch "test/Foo/BarSpec.hs"
      run ["test/Spec.hs", "", "out", "--no-main"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Spec where"
        , "import qualified FooSpec"
        , "import qualified Foo.BarSpec"
        , "import qualified Foo.Bar.BazSpec"
        , "import Test.Hspec.Discover"
        , "spec :: Spec"
        , "spec = " ++ unwords [
               "describe \"Foo\" FooSpec.spec"
          , ">> describe \"Foo.Bar\" Foo.BarSpec.spec"
          , ">> describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec"
          ]
        ]

    it "generates a test driver with hooks" $ do
      touch "test/FooSpec.hs"
      touch "test/Foo/Bar/BazSpec.hs"
      touch "test/Foo/BarSpec.hs"
      touch "test/Foo/SpecHook.hs"
      touch "test/SpecHook.hs"
      run ["test/Spec.hs", "", "out"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Main where"
        , "import qualified SpecHook"
        , "import qualified FooSpec"
        , "import qualified Foo.SpecHook"
        , "import qualified Foo.BarSpec"
        , "import qualified Foo.Bar.BazSpec"
        , "import Test.Hspec.Discover"
        , "main :: IO ()"
        , "main = hspec spec"
        , "spec :: Spec"
        , "spec = " ++ unwords [
               "(SpecHook.hook $ describe \"Foo\" FooSpec.spec"
          , ">> (Foo.SpecHook.hook $ describe \"Foo.Bar\" Foo.BarSpec.spec"
          , ">> describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec))"
          ]
        ]

    it "generates a test driver with hooks and wrap" $ do
      touch "test/SpecWrap.hs"
      touch "test/FooSpec.hs"
      touch "test/Foo/Bar/BazSpec.hs"
      touch "test/Foo/BarSpec.hs"
      touch "test/Foo/SpecHook.hs"
      touch "test/SpecHook.hs"
      run ["test/Spec.hs", "", "out"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Main where"
        , "import qualified SpecWrap"
        , "import qualified SpecHook"
        , "import qualified FooSpec"
        , "import qualified Foo.SpecHook"
        , "import qualified Foo.BarSpec"
        , "import qualified Foo.Bar.BazSpec"
        , "import Test.Hspec.Discover"
        , "main :: IO ()"
        , "main = hspec spec"
        , "spec :: Spec"
        , "spec = " ++ unwords [
               "(SpecHook.hook $ SpecWrap.wrapSpec (describe \"Foo\" FooSpec.spec)"
          , ">> (Foo.SpecHook.hook $ SpecWrap.wrapSpec (describe \"Foo.Bar\" Foo.BarSpec.spec)"
          , ">> SpecWrap.wrapSpec (describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec)))"
          ]
        ]

    it "generates a test driver for an empty directory" $ do
      touch "test/Foo/Bar/Baz/.placeholder"
      run ["test/Spec.hs", "", "out"]
      readFile "out" `shouldReturn` unlines [
          "{-# LINE 1 \"test/Spec.hs\" #-}"
        , "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -w -Wall -fno-warn-warnings-deprecations #-}"
        , "module Main where"
        , "import Test.Hspec.Discover"
        , "main :: IO ()"
        , "main = hspec spec"
        , "spec :: Spec"
        , "spec = return ()"
        ]

  describe "pathToModule" $ do
    it "derives module name from a given path" $ do
      pathToModule "test/Spec.hs" `shouldBe` "Spec"

  describe "driverWithFormatter" $ do
    it "generates a test driver that uses a custom formatter" $ do
      driverWithFormatter "Some.Module.formatter" "" `shouldBe` unlines [
          "import qualified Some.Module"
        , "main :: IO ()"
        , "main = hspecWithFormatter Some.Module.formatter spec"
        ]

  describe "moduleNameFromId" $ do
    it "returns the module name of a fully qualified identifier" $ do
      moduleNameFromId "Some.Module.someId" `shouldBe` "Some.Module"

  describe "importList" $ do
    it "generates imports for a list of specs" $ do
      importList (Just (Run.NoWrap, [Run.Spec "Foo", Run.Spec "Bar"])) "" `shouldBe` unlines [
          "import qualified FooSpec"
        , "import qualified BarSpec"
        ]

  describe "discover" $ do
    it "discovers spec files" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/FooSpec.hs"
        touch "test/BarSpec.hs"
        discover "test/Spec.hs" `shouldReturn` Just (Forest WithoutHook [Leaf "Bar", Leaf "Foo"])

    it "discovers nested spec files" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/Foo/BarSpec.hs"
        touch "test/Foo/BazSpec.hs"
        discover "test/Spec.hs" `shouldReturn` Just (Forest WithoutHook [Node "Foo" (Forest WithoutHook [Leaf "Bar", Leaf "Baz"])])

    it "discovers hooks" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/FooSpec.hs"
        touch "test/BarSpec.hs"
        touch "test/SpecHook.hs"
        discover "test/Spec.hs" `shouldReturn` Just (Forest WithHook [Leaf "Bar", Leaf "Foo"])

    it "discovers nested hooks" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/Foo/BarSpec.hs"
        touch "test/Foo/BazSpec.hs"
        touch "test/Foo/SpecHook.hs"
        discover "test/Spec.hs" `shouldReturn` Just (Forest WithoutHook [Node "Foo" (Forest WithHook [Leaf "Bar", Leaf "Baz"])])

    it "ignores invalid module names" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/barSpec.hs"
        discover "test/Spec.hs" `shouldReturn` Nothing

    it "ignores empty directories" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/Foo/.keep"
        discover "test/Spec.hs" `shouldReturn` Nothing

    it "ignores directories with extension" $ do
      inTempDirectory $ do
        touch "test/Spec.hs"
        touch "test/Foo.hs/BarSpec.hs"
        discover "test/Spec.hs" `shouldReturn` Nothing
