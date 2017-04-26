{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.Eval (
  EvalConfig(..)
, EvalTree
, EvalItem(..)
, runFormatter
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (Monad)
import qualified Test.Hspec.Core.Compat as M

import           Control.Monad (unless, when)
import qualified Control.Exception as E
import           Control.Concurrent

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class as M

import           Control.Monad.Trans.State hiding (State, state)
import           Control.Monad.Trans.Class

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Tree(..), Location, Progress, FailureReason, Result(..), ProgressCallback)
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format

-- for compatibility with GHC < 7.10.1
class (Functor m, Applicative m, M.Monad m) => Monad m
instance (Functor m, Applicative m, M.Monad m) => Monad m
class (Monad m, M.MonadIO m) => MonadIO m
instance (Monad m, M.MonadIO m) => MonadIO m

data EvalConfig m = EvalConfig {
  evalConfigFormat :: Format m
, evalConfigConcurrentJobs :: Int
, evalConfigFastFail :: Bool
}

data State m = State {
  stateConfig :: EvalConfig m
, stateSuccessCount :: Int
, statePendingCount :: Int
, stateFailures :: [Path]
}

type EvalM m a = StateT (State m) m a

increaseSuccessCount :: Monad m => EvalM m ()
increaseSuccessCount = modify $ \state -> state {stateSuccessCount = stateSuccessCount state + 1}

increasePendingCount :: Monad m => EvalM m ()
increasePendingCount = modify $ \state -> state {statePendingCount = statePendingCount state + 1}

addFailure :: Monad m => Path -> EvalM m ()
addFailure path = modify $ \state -> state {stateFailures = path : stateFailures state}

getFormat :: Monad m => (Format m -> a) -> EvalM m a
getFormat format = gets (format . evalConfigFormat . stateConfig)

reportSuccess :: Monad m => Path -> EvalM m ()
reportSuccess path = do
  increaseSuccessCount
  format <- getFormat formatSuccess
  lift (format path)

reportPending :: Monad m => Path -> Maybe String -> EvalM m ()
reportPending path reason = do
  increasePendingCount
  format <- getFormat formatPending
  lift (format path reason)

reportFailure :: Monad m => Maybe Location -> Path -> Either E.SomeException FailureReason -> EvalM m ()
reportFailure loc path err = do
  addFailure path
  format <- getFormat formatFailure
  lift $ format path loc err

groupStarted :: Monad m => [String] -> String -> EvalM m ()
groupStarted nesting name = do
  format <- getFormat formatGroupStarted
  lift $ format nesting name

groupDone :: Monad m => EvalM m ()
groupDone = getFormat formatGroupDone >>= lift

data EvalItem = EvalItem {
  evalItemDescription :: String
, evalItemLocation :: Maybe Location
, evalItemParallelize :: Bool
, evalItemAction :: ProgressCallback -> IO (Either E.SomeException Result)
}

type EvalTree = Tree (IO ()) EvalItem

runEvalM :: Monad m => EvalConfig m -> EvalM m () -> m (State m)
runEvalM config action = execStateT action (State config 0 0 [])

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: MonadIO m => EvalConfig m -> [EvalTree] -> IO (Int, [Path])
runFormatter config specs = do
  withTimer 0.05 $ \timer -> do
    jobsSem <- newQSem (evalConfigConcurrentJobs config)
    state <- formatRun format $ do
      runEvalM config (runFormatter_ timer config jobsSem specs)
    let
      failures = stateFailures state
      total = stateSuccessCount state + statePendingCount state + length failures
    return (total, reverse failures)
  where
    format = evalConfigFormat config

runFormatter_ :: forall m. MonadIO m => (IO Bool) -> EvalConfig m -> QSem -> [EvalTree] -> EvalM m ()
runFormatter_ timer config jobsSem specs = do
  chan <- liftIO newChan
  run jobsSem reportProgress chan specs
  where
    reportProgress :: Path -> ProgressCallback
    reportProgress path progress = do
      r <- timer
      when r (formatProgress format path progress)
    format = evalConfigFormat config

type ItemResult = Either E.SomeException Result

parallelize :: MonadIO m => QSem -> Bool -> (ProgressCallback -> IO a) -> ProgressCallback -> IO (m a)
parallelize jobsSem isParallelizable e
  | isParallelizable = runParallel jobsSem e
  | otherwise = runSequentially e

runSequentially :: MonadIO m => (ProgressCallback -> IO a) -> ProgressCallback -> IO (m a)
runSequentially e reportProgress = return $ do
  liftIO (e reportProgress)

data Parallel a = ReportProgress Progress | Return a

runParallel :: forall m a. MonadIO m => QSem -> (ProgressCallback -> IO a) -> ProgressCallback -> IO (m a)
runParallel jobsSem e reportProgress = do
  mvar <- newEmptyMVar
  _ <- forkIO $ E.bracket_ (waitQSem jobsSem) (signalQSem jobsSem) $ do
    let progressCallback = replaceMVar mvar . ReportProgress
    result <- e progressCallback
    replaceMVar mvar (Return result)
  return $ evalReport mvar
  where
    evalReport :: MVar (Parallel a) -> m a
    evalReport mvar = do
      r <- liftIO (takeMVar mvar)
      case r of
        ReportProgress p -> do
          liftIO $ reportProgress p
          evalReport mvar
        Return result -> return result

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

data Message m = Done | Run (EvalM m ())

run :: forall m. MonadIO m => QSem -> (Path -> ProgressCallback) -> Chan (Message m) -> [EvalTree] -> EvalM m ()
run jobsSem reportProgress chan specs = do
  liftIO $ do
    forM_ specs (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan)
  where
    defer :: EvalM m () -> IO ()
    defer = writeChan chan . Run

    runCleanup :: IO () -> Path -> EvalM m ()
    runCleanup action path = do
      r <- liftIO $ safeTry action
      either (reportFailure Nothing path . Left) return r

    queueSpec :: [String] -> EvalTree -> IO ()
    queueSpec rGroups (Node group xs) = do
      defer (groupStarted (reverse rGroups) group)
      forM_ xs (queueSpec (group : rGroups))
      defer groupDone
    queueSpec rGroups (NodeWithCleanup action xs) = do
      forM_ xs (queueSpec rGroups)
      defer (runCleanup action (reverse rGroups, "afterAll-hook"))
    queueSpec rGroups (Leaf e) =
      queueExample (reverse rGroups) e

    queueExample :: [String] -> EvalItem -> IO ()
    queueExample groups (EvalItem requirement loc isParallelizable e) = do
      action <- parallelize jobsSem isParallelizable e (reportProgress path)
      defer $ lift action >>= reportResult
      where
        path :: Path
        path = (groups, requirement)

        reportResult :: ItemResult -> EvalM m ()
        reportResult result = do
          case result of
            Right Success -> reportSuccess path
            Right (Pending reason) -> reportPending path reason
            Right (Failure loc_ err) -> reportFailure (loc_ <|> loc) path (Right err)
            Left err -> reportFailure loc path (Left  err)

processMessages :: MonadIO m => IO (Message m) -> EvalM m ()
processMessages getMessage = do
  fastFail <- gets (evalConfigFastFail . stateConfig)
  start fastFail
  where
    start fastFail = go
      where
        go = liftIO getMessage >>= \m -> case m of
          Run action -> do
            action
            hasFailures <- (not . null) <$> gets stateFailures
            let stopNow = fastFail && hasFailures
            unless stopNow go
          Done -> return ()
