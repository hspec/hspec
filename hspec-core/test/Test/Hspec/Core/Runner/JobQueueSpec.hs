module Test.Hspec.Core.Runner.JobQueueSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Concurrent

import           Test.Hspec.Core.Runner.JobQueue

spec :: Spec
spec = do
  describe "enqueueJob" $ do
    let
      waitFor job = job (\ _ -> pass) >>= either throwIO return

    context "with Sequential" $ do
      it "runs actions sequentially" $ do
        withJobQueue 10 $ \ queue -> do
          ref <- newIORef []
          jobA <- enqueueJob queue Sequential $ \ _ -> modifyIORef ref (23 :)
          jobB <- enqueueJob queue Sequential $ \ _ -> modifyIORef ref (42 :)
          waitFor jobB
          readIORef ref `shouldReturn` [42 :: Int]
          waitFor jobA
          readIORef ref `shouldReturn` [23, 42]

    context "with Concurrent" $ do
      it "runs actions concurrently" $ do
        withJobQueue 10 $ \ queue -> do
          barrierA <- newEmptyMVar
          barrierB <- newEmptyMVar

          jobA <- enqueueJob queue Concurrent $ \ _ -> do
            putMVar barrierB ()
            takeMVar barrierA

          jobB <- enqueueJob queue Concurrent $ \ _ -> do
            putMVar barrierA ()
            takeMVar barrierB

          timeout (0.1 :: Seconds) $ do
            waitFor jobA
            waitFor jobB
          `shouldReturn` Just ()
