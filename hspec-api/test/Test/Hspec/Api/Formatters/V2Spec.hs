module Test.Hspec.Api.Formatters.V2Spec (spec) where

import           Test.Hspec
import           Test.Hspec.Runner

import           Data.IORef
import           Control.Monad.IO.Class

import           Test.Hspec.Api.Formatters.V2

spec :: Spec
spec = do
  describe "useFormatter" $ do
    it "sets a formatter to be used with a given config" $ do
      ref <- newIORef "NAY!"
      let
        formatter :: Formatter
        formatter = silent { formatterItemStarted = \ _ -> liftIO $ writeIORef ref "YAY!" }
      hspecWith (useFormatter ("my-formatter", formatter) defaultConfig) $ it "" True
      readIORef ref `shouldReturn` "YAY!"
