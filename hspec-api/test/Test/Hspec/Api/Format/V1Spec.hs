module Test.Hspec.Api.Format.V1Spec (spec) where

import           Test.Hspec
import           Test.Hspec.Runner

import           Data.IORef

import           Test.Hspec.Api.Format.V1

spec :: Spec
spec = do
  describe "useFormatter" $ do
    it "sets a formatter to be used with a given config" $ do
      ref <- newIORef "NAY!"
      let
        formatter :: Format
        formatter event = case event of
          ItemDone {} -> writeIORef ref "YAY!"
          _ -> return ()

      hspecWith (useFormatter ("my-formatter", \ _ -> return formatter) defaultConfig) $ it "" True
      readIORef ref `shouldReturn` "YAY!"
