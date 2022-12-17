{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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
, Tree(..)
, EvalItem(..)
, runFormatter
#ifdef TEST
, mergeResults
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
import           Test.Hspec.Core.Spec (Progress, FailureReason(..), Result(..), ResultStatus(..), ProgressCallback)
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format (Format)
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example.Location
import           Test.Hspec.Core.Example (safeEvaluateResultStatus)

import qualified NonEmpty
import           NonEmpty (NonEmpty(..))

data Tree c a =
    Node String (NonEmpty (Tree c a))
  | NodeWithCleanup (Maybe (String, Location)) c (NonEmpty (Tree c a))
  | Leaf a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- for compatibility with GHC < 7.10.1
type Monad m = (Functor m, Applicative m, M.Monad m)
type MonadIO m = (Monad m, M.MonadIO m)

data EvalConfig = EvalConfig {
  evalConfigFormat :: Format
, evalConfigConcurrentJobs :: Int
, evalConfigFailFast :: Bool
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
      runReaderT (run . applyCleanup $ map (fmap (fmap (. reportProgress timer) . snd)) runningSpecs) (Env config ref) `E.finally` do
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
, itemAction :: a
} deriving Functor

type Job m progress a = (progress -> m ()) -> m a

type RunningItem = Item (Path -> IO (Seconds, Result))
type RunningTree c = Tree c RunningItem

applyCleanup :: [RunningTree (IO ())] -> [RunningTree ()]
applyCleanup = map go
  where
    go t = case t of
      Node label xs -> Node label (go <$> xs)
      NodeWithCleanup loc cleanup xs -> NodeWithCleanup loc () (addCleanupToLastLeaf loc cleanup $ go <$> xs)
      Leaf a -> Leaf a

addCleanupToLastLeaf :: Maybe (String, Location) -> IO () -> NonEmpty (RunningTree ()) -> NonEmpty (RunningTree ())
addCleanupToLastLeaf loc cleanup = go
  where
    go = NonEmpty.reverse . mapHead goNode . NonEmpty.reverse

    goNode node = case node of
      Node description xs -> Node description (go xs)
      NodeWithCleanup loc_ () xs -> NodeWithCleanup loc_ () (go xs)
      Leaf item -> Leaf (addCleanupToItem loc cleanup item)

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead f xs = case xs of
  y :| ys -> f y :| ys

addCleanupToItem :: Maybe (String, Location) -> IO () -> RunningItem -> RunningItem
addCleanupToItem loc cleanup item = item {
  itemAction = \ path -> do
    (t1, r1) <- itemAction item path
    (t2, r2) <- measure $ safeEvaluateResultStatus (cleanup >> return Success)
    let t = t1 + t2
    return (t, mergeResults loc r1 r2)
}

mergeResults :: Maybe (String, Location) -> Result -> ResultStatus -> Result
mergeResults mCallSite (Result info r1) r2 = Result info $ case (r1, r2) of
  (_, Success) -> r1
  (Failure{}, _) -> r1
  (Pending{}, Pending{}) -> r1
  (Success, Pending{}) -> r2
  (_, Failure mLoc err) -> Failure (mLoc <|> hookLoc) $ case err of
    Error message e -> Error (message <|> hookFailed) e
    _ -> err
  where
    hookLoc = snd <$> mCallSite
    hookFailed = case mCallSite of
      Just (name, _) -> Just $ "in " ++ name ++ "-hook:"
      Nothing -> Nothing

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

parallelize :: MonadIO m => Semaphore -> Bool -> Job IO progress a -> IO (Async (), Job m progress (Seconds, a))
parallelize sem isParallelizable
  | isParallelizable = runParallel sem
  | otherwise = runSequentially

runSequentially :: MonadIO m => Job IO progress a -> IO (Async (), Job m progress (Seconds, a))
runSequentially action = do
  mvar <- newEmptyMVar
  (asyncAction, evalAction) <- runParallel (Semaphore (takeMVar mvar) pass) action
  return (asyncAction, \ notifyPartial -> liftIO (putMVar mvar ()) >> evalAction notifyPartial)

data Parallel progress a = Partial progress | Return a

runParallel :: forall m progress a. MonadIO m => Semaphore -> Job IO progress a -> IO (Async (), Job m progress (Seconds, a))
runParallel Semaphore{..} action = do
  mvar <- newEmptyMVar
  asyncAction <- async $ E.bracket_ semaphoreWait semaphoreSignal (worker mvar)
  return (asyncAction, eval mvar)
  where
    worker :: MVar (Parallel progress (Seconds, a)) -> IO ()
    worker mvar = do
      let partialCallback = replaceMVar mvar . Partial
      result <- measure $ action partialCallback
      replaceMVar mvar (Return result)

    eval :: MVar (Parallel progress (Seconds, a)) -> (progress -> m ()) -> m (Seconds, a)
    eval mvar notifyPartial = do
      r <- liftIO (takeMVar mvar)
      case r of
        Partial p -> do
          notifyPartial p
          eval mvar notifyPartial
        Return result -> return result

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

run :: [RunningTree ()] -> EvalM ()
run specs = do
  failFast <- asks (evalConfigFailFast . envConfig)
  sequenceActions failFast (concatMap foldSpec specs)
  where
    foldSpec :: RunningTree () -> [EvalM ()]
    foldSpec = foldTree FoldTree {
      onGroupStarted = groupStarted
    , onGroupDone = groupDone
    , onCleanup = runCleanup
    , onLeafe = evalItem
    }

    runCleanup :: Maybe (String, Location) -> [String] -> () -> EvalM ()
    runCleanup _loc _groups = return

    evalItem :: [String] -> RunningItem -> EvalM ()
    evalItem groups (Item requirement loc action) = do
      reportItem path loc $ lift (action path)
      where
        path :: Path
        path = (groups, requirement)

data FoldTree c a r = FoldTree {
  onGroupStarted :: Path -> r
, onGroupDone :: Path -> r
, onCleanup :: Maybe (String, Location) -> [String] -> c -> r
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
sequenceActions failFast = go
  where
    go :: [EvalM ()] -> EvalM ()
    go [] = pass
    go (action : actions) = do
      action
      stopNow <- case failFast of
        False -> return False
        True -> any itemIsFailure <$> getResults
      unless stopNow (go actions)

    itemIsFailure :: (Path, Format.Item) -> Bool
    itemIsFailure = isFailure . Format.itemResult . snd
      where
        isFailure r = case r of
          Format.Success{} -> False
          Format.Pending{} -> False
          Format.Failure{} -> True
