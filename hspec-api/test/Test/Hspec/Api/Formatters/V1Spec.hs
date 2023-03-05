module Test.Hspec.Api.Formatters.V1Spec (spec) where

import           Test.Hspec
import           Test.Hspec.Runner

import           Data.IORef
import           Control.Monad.IO.Class

import           Test.Hspec.Api.Formatters.V1

spec :: Spec
spec = do
  describe "setFormatter" $ do
    it "sets a formatter to be used with a given config" $ do
      ref <- newIORef "NAY!"
      let
        formatter :: Formatter
        formatter = silent { exampleStarted = \ _ -> liftIO $ writeIORef ref "YAY!" }
      hspecWith (setFormatter formatter defaultConfig) $ it "" True
      readIORef ref `shouldReturn` "YAY!"
