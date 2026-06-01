{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Runner.JobQueue (
  JobQueue
, Open
, Final
, Concurrency(..)
, Job
, new
, finalize
, run
, enqueue
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Concurrent
import           Control.Concurrent.Async (Async, async, cancelMany)

import           Control.Monad.IO.Class

import           Test.Hspec.Core.Util (safeTry)

data Concurrency = Sequential | Concurrent

data Open
data Final

type QueuedJob progress a = IO ()

newtype JobQueue final progress a = JobQueue (IORef [IO ()])

new :: IO (JobQueue Open progress a)
new = JobQueue <$> newIORef []

enqueueJob :: JobQueue Open progress a -> QueuedJob progress a -> IO ()
enqueueJob (JobQueue ref) job = modifyIORef' ref (job :)

finalize :: JobQueue Open progress a -> IO (JobQueue Final progress a)
finalize (JobQueue ref) = do
  jobs <- reverse <$> atomicSwapIORef ref []
  JobQueue <$> newIORef jobs

dequeue :: JobQueue Final progress a -> IO (Maybe (QueuedJob progress a))
dequeue (JobQueue ref) = atomicModifyIORef ref $ \ case
  job : jobs -> (jobs, Just job)
  [] -> ([], Nothing)

run :: Int -> JobQueue Final progress a -> IO r -> IO r
run concurrency jobs action = do
  let
    worker :: IO ()
    worker = dequeue jobs >>= \ case
      Nothing -> pass
      Just item -> item >> worker

    startWorkers :: IO [Async ()]
    startWorkers = replicateM concurrency $ async worker

  bracket startWorkers cancelMany $ \ _ -> do
    action

type Job progress a = (progress -> IO ()) -> IO a

data Result progress a = Partial progress | Final (Either SomeException a)

enqueue :: JobQueue Open progress a -> Concurrency -> Job progress a -> IO (Job progress (Either SomeException a))
enqueue queue = \ case
  Sequential -> runSequential
  Concurrent -> runConcurrent queue

runSequential :: Job progress a -> IO (Job progress (Either SomeException a))
runSequential action = return $ safeTry . action

runConcurrent :: JobQueue Open progress a -> Job progress a -> IO (Job progress (Either SomeException a))
runConcurrent queue action = do
  result <- newEmptyMVar
  let
    partialResult = replaceMVar result . Partial
  enqueueJob queue $ do
    safeTry (action partialResult) >>= replaceMVar result . Final
  return $ waitForResult result

waitForResult :: forall progress a. MVar (Result progress a) -> Job progress (Either SomeException a)
waitForResult result notifyPartial = loop
  where
    loop :: IO (Either SomeException a)
    loop = do
      r <- liftIO (takeMVar result)
      case r of
        Partial progress -> notifyPartial progress >> loop
        Final finalResult -> return finalResult

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p
