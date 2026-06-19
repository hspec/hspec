{-# LANGUAGE TypeApplications #-}
module Test.Hspec.Core.Runner.JobQueueSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Concurrent
import           Control.Concurrent.Async (ExceptionInLinkedThread(..))

import           Test.Hspec.Core.Runner.JobQueue as JobQueue

spec :: Spec
spec = do
  describe "enqueue" $ do
    let
      waitFor :: Result () () -> Report () ()
      waitFor = waitForWith NoAbortEarly

      waitForWith :: AbortEarly -> Result () () -> Report () ()
      waitForWith abort job = ReportResult job return (either throwIO (\ () -> return abort) . snd)

    context "with Sequential" $ do
      it "runs actions sequentially" $ do
        queue <- JobQueue.new

        ref <- newIORef []
        jobA <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref (23 :)
        jobB <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref (42 :)

        jobs <- JobQueue.finalize queue

        JobQueue.run 10 jobs [
            waitFor jobB
          , Report $ readIORef ref `shouldReturn` [42 :: Int]
          , waitFor jobA
          , Report $ readIORef ref `shouldReturn` [23, 42]
          , Report $ modifyIORef ref (65 :)
          ]
        readIORef ref `shouldReturn` [65, 23, 42]

      it "strictly respects AbortEarly" $ do
        queue <- JobQueue.new

        ref <- newIORef @Int 0
        jobA <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref succ
        jobB <- JobQueue.enqueue queue Sequential $ \ _ -> modifyIORef ref succ

        jobs <- JobQueue.finalize queue

        JobQueue.run 1 jobs [
            waitForWith AbortEarly jobA
          , waitFor jobB
          ]
        readIORef ref `shouldReturn` 1

      it "propagates exceptions" $ do
        timeout (0.1 :: Seconds) $ do
          queue <- JobQueue.new

          jobA <- JobQueue.enqueue queue Sequential $ \ _ -> pass
          jobB <- JobQueue.enqueue queue Sequential $ \ _ -> pass

          jobs <- JobQueue.finalize queue

          JobQueue.run 1 jobs [
              waitFor jobA
            , waitFor jobB
            , Report $ throwIO DivideByZero
            ]
          `shouldThrow` \ (ExceptionInLinkedThread _ _) -> True
        `shouldReturn` Just ()

    context "with Concurrent" $ do
      it "runs actions concurrently" $ do
        queue <- JobQueue.new

        barrierA <- newEmptyMVar
        barrierB <- newEmptyMVar

        doneA <- newEmptyMVar
        doneB <- newEmptyMVar

        jobA <- JobQueue.enqueue queue Concurrent $ \ _ -> do
          putMVar barrierB ()
          takeMVar barrierA
          putMVar doneA ()
        jobB <- JobQueue.enqueue queue Concurrent $ \ _ -> do
          putMVar barrierA ()
          takeMVar barrierB
          putMVar doneB ()

        jobs <- JobQueue.finalize queue

        timeout (0.1 :: Seconds) $ do
          JobQueue.run 10 jobs [
              waitFor jobA
            , waitFor jobB
            ]
          takeMVar doneA
          takeMVar doneB
        `shouldReturn` Just ()
