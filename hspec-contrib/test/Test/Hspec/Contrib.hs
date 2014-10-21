{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Test.Hspec.Core.Conrib (main, spec) where

import           Data.IORef

import qualified Test.Hspec.Core.Type as H hiding (describe, it)
import qualified Test.Hspec as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "retry test" $ do
    ref <- runIO $ newIORef (0::Int)
    it "retry 11 times, then check the value" $ do
      let incr :: IO Int
          incr = do
            val <- readIORef ref
            writeIORef ref (val+1)
            return val
      silence . H.hspec $ do
        H.it "retry 11 times" $ do
          H.retryWith 11 $ do
            incr `shouldReturn` (10::Int)
