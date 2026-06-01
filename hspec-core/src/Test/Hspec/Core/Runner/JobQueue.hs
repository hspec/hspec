{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Runner.JobQueue (
  Concurrency(..)
, JobQueue
, Open
, Final
, new
, finalize
, run
, enqueue
, Report(..)
, AbortEarly(..)
, Result
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (all)

import           Foreign.Marshal.Utils (withMany)
import           Control.Concurrent
import           Control.Concurrent.Async (Async, withAsync, link, wait, cancelMany)

import           Test.Hspec.Core.Util (safeTry)

data Concurrency = Sequential | Concurrent

data Open
data Final

type QueuedJob progress a = IO (Continuation progress a)

newtype JobQueue final progress a = JobQueue (IORef [QueuedJob progress a])

new :: IO (JobQueue Open progress a)
new = JobQueue <$> newIORef []

enqueueJob :: JobQueue Open progress a -> QueuedJob progress a -> IO ()
enqueueJob (JobQueue ref) job = modifyIORef' ref (job :)

finalize :: JobQueue Open progress a -> IO (JobQueue Final progress a)
finalize (JobQueue ref) = do
  jobs <- reverse <$> atomicSwapIORef ref []
  JobQueue <$> newIORef jobs

dequeue :: JobQueue Final progress a -> IO (Maybe (QueuedJob progress a))
dequeue (JobQueue ref) = atomicModifyIORef' ref $ \ case
  job : jobs -> (jobs, Just job)
  [] -> ([], Nothing)

data Done = Abort | Done

run :: forall progress a. Int -> JobQueue Final progress a -> [Report progress a] -> IO ()
run concurrency jobs items = do
  parent <- newEmptyMVar
  let
    worker :: Continuation progress a -> IO ()
    worker = workerThread parent jobs

    workers :: [IO ()]
    workers = worker (ReportingThread items) : replicate (concurrency - 1) (worker WorkerThread)

  withAsyncs workers $ \ threads -> do
    mapM_ link threads
    takeMVar parent >>= \ case
      Abort -> cancelMany threads
      Done -> mapM_ wait threads

type Job progress a = (progress -> IO ()) -> IO a

data Result progress a =
    SequentialResult (Job progress a)
  | ConcurrentResult (IORef (ConcurrentJobState progress a))

enqueue :: JobQueue Open progress a -> Concurrency -> Job progress a -> IO (Result progress a)
enqueue queue concurrency job = case concurrency of
  Sequential -> return $ SequentialResult job
  Concurrent -> enqueueConcurrent queue job

data ConcurrentJobState progress a =
    Running
  | Reporting [Report progress a] (progress -> IO ())
  | Completed (Either SomeException a)

enqueueConcurrent :: forall progress a. JobQueue Open progress a -> Job progress a -> IO (Result progress a)
enqueueConcurrent queue job = do
  ref <- newIORef Running

  let
    reportProgress :: progress -> IO ()
    reportProgress progress = atomicReadIORef ref >>= \ case
      Reporting _ report -> report progress
      _ -> pass

    reportResult :: Either SomeException a -> IO (Continuation progress a)
    reportResult result = atomicSwapIORef ref (Completed result) >>= \ case
      Running -> return WorkerThread
      Reporting items _ -> return $ ReportingThread items
      Completed _ -> thisCanNeverHappen "job completed twice"

  enqueueJob queue $ do
    safeTry (job reportProgress) >>= reportResult

  return $ ConcurrentResult ref

data Report progress a =
    Report (IO ())
  | ReportResult {
      _resultAction :: Result progress a
    , _reportProgress :: progress -> IO ()
    , _reportResult :: Either SomeException a -> IO AbortEarly
    }

data AbortEarly = NoAbortEarly | AbortEarly

data Continuation progress a = WorkerThread | ReportingThread [Report progress a]

workerThread :: forall progress a. MVar Done -> JobQueue Final progress a -> Continuation progress a -> IO ()
workerThread parent jobs = loop
  where
    loop :: Continuation progress a -> IO ()
    loop = \ case
      WorkerThread -> dequeue jobs >>= \ case
        Nothing -> pass
        Just job -> job >>= loop
      ReportingThread [] -> done
      ReportingThread all@(item : items) -> case item of
        Report action -> do
          action >> keepReporting
        ReportResult result reportProgress (checkAbort -> reportResult) -> case result of
          SequentialResult action -> do
            r <- safeTry (action reportProgress)
            reportResult r keepReporting
          ConcurrentResult ref -> passReportingResponsibility ref (Reporting all reportProgress) >>= \ case
            Running -> continueAsWorkerThread
            Completed r -> reportResult r keepReporting
            Reporting _ _ -> thisCanNeverHappen "more than one reporting thread"
        where
          passReportingResponsibility :: IORef s -> s -> IO s
          passReportingResponsibility = atomicSwapIORef

          continueAsWorkerThread :: IO ()
          continueAsWorkerThread = loop WorkerThread

          keepReporting :: IO ()
          keepReporting = loop (ReportingThread items)

    checkAbort :: (r -> IO AbortEarly) -> r -> IO () -> IO ()
    checkAbort report r continue = report r >>= \ case
      NoAbortEarly -> continue
      AbortEarly -> abort

    abort :: IO ()
    abort = putMVar parent Abort

    done :: IO ()
    done = putMVar parent Done

withAsyncs :: forall a r. [IO a] -> ([Async a] -> IO r) -> IO r
withAsyncs = withMany withAsync

thisCanNeverHappen :: HasCallStack => String -> a
thisCanNeverHappen = error
