module Test.Hspec.Core.Runner.JobQueueSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Concurrent

import           Test.Hspec.Core.Runner.JobQueue

spec :: Spec
spec = do
  describe "enqueueJob" $ do
    context "with Sequential" $ do
      it "runs actions sequentially" $ do
        withJobQueue 10 $ \ queue -> do
          ref <- newIORef []
          waitForA <- enqueueJob queue Sequential $ \ _ -> modifyIORef ref (23 :)
          waitForB <- enqueueJob queue Sequential $ \ _ -> modifyIORef ref (42 :)
          waitForB (\ _ -> pass)
          readIORef ref `shouldReturn` [42 :: Int]
          waitForA (\ _ -> pass)
          readIORef ref `shouldReturn` [23, 42]

    context "with Concurrent" $ do
      it "runs actions concurrently" $ do
        withJobQueue 10 $ \ queue -> do
          barrierA <- newEmptyMVar
          barrierB <- newEmptyMVar

          waitForA <- enqueueJob queue Concurrent $ \ _ -> do
            putMVar barrierB ()
            takeMVar barrierA

          waitForB <- enqueueJob queue Concurrent $ \ _ -> do
            putMVar barrierA ()
            takeMVar barrierB

          timeout (0.1 :: Seconds) $ do
            waitForA (\ _ -> pass)
            waitForB (\ _ -> pass)
          `shouldReturn` Just ()
