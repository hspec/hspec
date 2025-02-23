module Test.Hspec.CISpec (spec) where

import           Prelude ()
import           Helper

import           Mock
import qualified Test.Hspec.Core.Spec as H

import qualified Test.Hspec.CI as CI

spec :: Spec
spec = do
  let
    hspec args = withArgs args . hspecSilent

  context "with --tags" $ do
    let
      run args = do
        foo <- newMock
        bar <- newMock
        hspec args $ do
          CI.use
          H.it "foo" >>> CI.only $ mockAction foo
          H.it "bar" $ mockAction bar
        (,) <$> mockCounter foo <*> mockCounter bar

    it "" $ do
      run [] `shouldReturn` (0, 1)

    it "" $ do
      run ["--ci"] `shouldReturn` (1, 1)

    context "" $ do
      around_ (withEnvironment [("CI", "true")]) $ do
        it "" $ do
          withEnvironment [("CI", "true")] $ do
            run [] `shouldReturn` (1, 1)

        it "" $ do
          run ["--no-ci"] `shouldReturn` (0, 1)
