module Test.Hspec.Core.Runner.JobQueueSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Concurrent

import           Test.Hspec.Core.Runner.JobQueue as JobQueue

spec :: Spec
spec = do
  describe "enqueueJob" $ do
    let
      waitFor job = job (\ _ -> pass) >>= either throwIO return

    context "with Sequential" $ do
      it "runs actions sequentially" $ do
        queue <- JobQueue.new

        ref <- newIORef []
        jobA <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref (23 :)
        jobB <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref (42 :)

        jobs <- JobQueue.finalize queue

        JobQueue.run 10 jobs $ do
          waitFor jobB
          readIORef ref `shouldReturn` [42 :: Int]
          waitFor jobA
          readIORef ref `shouldReturn` [23, 42]

    context "with Concurrent" $ do
      it "runs actions concurrently" $ do
        queue <- JobQueue.new

        barrierA <- newEmptyMVar
        barrierB <- newEmptyMVar
        jobA <- JobQueue.enqueue queue Concurrent $ \ _ -> do
          putMVar barrierB ()
          takeMVar barrierA
        jobB <- JobQueue.enqueue queue Concurrent $ \ _ -> do
          putMVar barrierA ()
          takeMVar barrierB

        jobs <- JobQueue.finalize queue

        JobQueue.run 10 jobs $ do
          timeout (0.1 :: Seconds) $ do
            waitFor jobA
            waitFor jobB
          `shouldReturn` Just ()
