{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.Eval (
  EvalConfig(..)
, EvalTree
, EvalItem(..)
, runFormatter
, resultItemIsFailure
#ifdef TEST
, runSequentially
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (Monad)
import qualified Test.Hspec.Core.Compat as M

import qualified Control.Exception as E
import           Control.Concurrent
import           Control.Concurrent.Async hiding (cancel)

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class as M

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Tree(..), Progress, FailureReason(..), Result(..), ResultStatus(..), ProgressCallback)
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format (Format)
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example.Location
import           Test.Hspec.Core.Example (safeEvaluate)

-- for compatibility with GHC < 7.10.1
type Monad m = (Functor m, Applicative m, M.Monad m)
type MonadIO m = (Monad m, M.MonadIO m)

data EvalConfig = EvalConfig {
  evalConfigFormat :: Format
, evalConfigConcurrentJobs :: Int
, evalConfigFastFail :: Bool
}

data Env = Env {
  envConfig :: EvalConfig
, envResults :: IORef [(Path, Format.Item)]
}

formatEvent :: Format.Event -> EvalM ()
formatEvent event = do
  format <- asks $ evalConfigFormat . envConfig
  liftIO $ format event

type EvalM = ReaderT Env IO

addResult :: Path -> Format.Item -> EvalM ()
addResult path item = do
  ref <- asks envResults
  liftIO $ modifyIORef ref ((path, item) :)

getResults :: EvalM [(Path, Format.Item)]
getResults = reverse <$> (asks envResults >>= liftIO . readIORef)

reportItem :: Path -> Maybe Location -> EvalM (Seconds, Result)  -> EvalM ()
reportItem path loc action = do
  reportItemStarted path
  action >>= reportResult path loc

reportItemStarted :: Path -> EvalM ()
reportItemStarted = formatEvent . Format.ItemStarted

reportItemDone :: Path -> Format.Item -> EvalM ()
reportItemDone path item = do
  addResult path item
  formatEvent $ Format.ItemDone path item

reportResult :: Path -> Maybe Location -> (Seconds, Result) -> EvalM ()
reportResult path loc (duration, result) = do
  case result of
    Result info status -> reportItemDone path $ Format.Item loc duration info $ case status of
      Success                      -> Format.Success
      Pending loc_ reason          -> Format.Pending loc_ reason
      Failure loc_ err@(Error _ e) -> Format.Failure (loc_ <|> extractLocation e) err
      Failure loc_ err             -> Format.Failure loc_ err

groupStarted :: Path -> EvalM ()
groupStarted = formatEvent . Format.GroupStarted

groupDone :: Path -> EvalM ()
groupDone = formatEvent . Format.GroupDone

data EvalItem = EvalItem {
  evalItemDescription :: String
, evalItemLocation :: Maybe Location
, evalItemParallelize :: Bool
, evalItemAction :: ProgressCallback -> IO Result
}

type EvalTree = Tree (IO ()) EvalItem

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: EvalConfig -> [EvalTree] -> IO ([(Path, Format.Item)])
runFormatter config specs = do
  ref <- newIORef []

  let
    start = parallelizeTree (evalConfigConcurrentJobs config) specs
    cancel = cancelMany . concatMap toList . map (fmap fst)

  E.bracket start cancel $ \ runningSpecs -> do
    withTimer 0.05 $ \ timer -> do

      format Format.Started
      runReaderT (run $ map (fmap (fmap (. reportProgress timer) . snd)) runningSpecs) (Env config ref) `E.finally` do
        results <- reverse <$> readIORef ref
        format (Format.Done results)

      results <- reverse <$> readIORef ref
      return results
  where
    format = evalConfigFormat config

    reportProgress timer path progress = do
      r <- timer
      when r $ do
        format (Format.Progress path progress)

cancelMany :: [Async a] -> IO ()
cancelMany asyncs = do
  mapM_ (killThread . asyncThreadId) asyncs
  mapM_ waitCatch asyncs

data Item a = Item {
  _itemDescription :: String
, _itemLocation :: Maybe Location
, _itemAction :: a
} deriving Functor

type Job m p a = (p -> m ()) -> m a

type RunningItem m = Item (Path -> m (Seconds, Result))
type RunningTree m = Tree (IO ()) (RunningItem m)

type RunningItem_ m = (Async (), Item (Job m Progress (Seconds, Result)))
type RunningTree_ m = Tree (IO ()) (RunningItem_ m)

data Semaphore = Semaphore {
  semaphoreWait :: IO ()
, semaphoreSignal :: IO ()
}

parallelizeTree :: MonadIO m => Int -> [EvalTree] -> IO [RunningTree_ m]
parallelizeTree n specs = do
  sem <- newQSem n
  mapM (traverse $ parallelizeItem sem) specs

parallelizeItem :: MonadIO m => QSem -> EvalItem -> IO (RunningItem_ m)
parallelizeItem sem EvalItem{..} = do
  (asyncAction, evalAction) <- parallelize (Semaphore (waitQSem sem) (signalQSem sem)) evalItemParallelize (interruptible . evalItemAction)
  return (asyncAction, Item evalItemDescription evalItemLocation evalAction)

parallelize :: MonadIO m => Semaphore -> Bool -> Job IO p a -> IO (Async (), Job m p (Seconds, a))
parallelize sem isParallelizable
  | isParallelizable = runParallel sem
  | otherwise = runSequentially

runSequentially :: MonadIO m => Job IO p a -> IO (Async (), Job m p (Seconds, a))
runSequentially action = do
  mvar <- newEmptyMVar
  (asyncAction, evalAction) <- runParallel (Semaphore (takeMVar mvar) (return ())) action
  return (asyncAction, \ notifyPartial -> liftIO (putMVar mvar ()) >> evalAction notifyPartial)

data Parallel p a = Partial p | Return a

runParallel :: forall m p a. MonadIO m => Semaphore -> Job IO p a -> IO (Async (), Job m p (Seconds, a))
runParallel Semaphore{..} action = do
  mvar <- newEmptyMVar
  asyncAction <- async $ E.bracket_ semaphoreWait semaphoreSignal (worker mvar)
  return (asyncAction, eval mvar)
  where
    worker mvar = do
      let partialCallback = replaceMVar mvar . Partial
      result <- measure $ action partialCallback
      replaceMVar mvar (Return result)

    eval :: MVar (Parallel p (Seconds, a)) -> (p -> m ()) -> m (Seconds, a)
    eval mvar notifyPartial = do
      r <- liftIO (takeMVar mvar)
      case r of
        Partial p -> do
          notifyPartial p
          eval mvar notifyPartial
        Return result -> return result

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

run :: [RunningTree IO] -> EvalM ()
run specs = do
  fastFail <- asks (evalConfigFastFail . envConfig)
  sequenceActions fastFail (concatMap foldSpec specs)
  where
    foldSpec :: RunningTree IO -> [EvalM ()]
    foldSpec = foldTree FoldTree {
      onGroupStarted = groupStarted
    , onGroupDone = groupDone
    , onCleanup = runCleanup
    , onLeafe = evalItem
    }

    runCleanup :: Maybe Location -> [String] -> IO () -> EvalM ()
    runCleanup loc groups action = do
      r <- liftIO $ measure $ safeEvaluate (action >> return (Result "" Success))
      case r of
        (_, Result "" Success) -> return ()
        _ -> reportItem path loc (return r)
      where
        path = (groups, "afterAll-hook")

    evalItem :: [String] -> RunningItem IO -> EvalM ()
    evalItem groups (Item requirement loc action) = do
      reportItem path loc $ lift (action path)
      where
        path :: Path
        path = (groups, requirement)

data FoldTree c a r = FoldTree {
  onGroupStarted :: Path -> r
, onGroupDone :: Path -> r
, onCleanup :: Maybe Location -> [String] -> c -> r
, onLeafe :: [String] -> a -> r
}

foldTree :: FoldTree c a r -> Tree c a -> [r]
foldTree FoldTree{..} = go []
  where
    go rGroups (Node group xs) = start : children ++ [done]
      where
        path = (reverse rGroups, group)
        start = onGroupStarted path
        children = concatMap (go (group : rGroups)) xs
        done =  onGroupDone path
    go rGroups (NodeWithCleanup loc action xs) = children ++ [cleanup]
      where
        children = concatMap (go rGroups) xs
        cleanup = onCleanup loc (reverse rGroups) action
    go rGroups (Leaf a) = [onLeafe (reverse rGroups) a]

sequenceActions :: Bool -> [EvalM ()] -> EvalM ()
sequenceActions fastFail = go
  where
    go :: [EvalM ()] -> EvalM ()
    go [] = return ()
    go (action : actions) = do
      action
      hasFailures <- any resultItemIsFailure <$> getResults
      let stopNow = fastFail && hasFailures
      unless stopNow (go actions)

resultItemIsFailure :: (Path, Format.Item) -> Bool
resultItemIsFailure = isFailure . Format.itemResult . snd
  where
    isFailure r = case r of
      Format.Success{} -> False
      Format.Pending{} -> False
      Format.Failure{} -> True
