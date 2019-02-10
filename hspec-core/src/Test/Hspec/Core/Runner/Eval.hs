{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Hspec.Core.Runner.Eval (
  EvalConfig(..)
, EvalTree
, EvalItem(..)
, runFormatter
#ifdef TEST
, AsyncCell(..)
, Parallel(..)
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

import           Control.Monad.Trans.State hiding (State, state)
import           Control.Monad.Trans.Class

import           Data.Bifunctor
import           Data.Maybe (catMaybes, fromMaybe, maybeToList)
import qualified Data.Semigroup as S

import           Test.Hspec.Core.Example (LifeCycle(..), LifeCycleCallback, Progress)
import           Test.Hspec.Core.Runner.Eval.Types
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Tree(..), FailureReason(..), Result(..), ResultStatus(..))
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format (Format(..))
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example.Location

-- for compatibility with GHC < 7.10.1
type Monad m = (Functor m, Applicative m, M.Monad m)
type MonadIO m = (Monad m, M.MonadIO m)

increaseSuccessCount :: Monad m => EvalM m ()
increaseSuccessCount = EvalM $ modify $ \state -> state {stateSuccessCount = stateSuccessCount state + 1}

increasePendingCount :: Monad m => EvalM m ()
increasePendingCount = EvalM $ modify $ \state -> state {statePendingCount = statePendingCount state + 1}

addFailure :: Monad m => Path -> EvalM m ()
addFailure path = EvalM $ modify $ \state -> state {stateFailures = path : stateFailures state}

getFormat :: Monad m => (Format m -> a) -> EvalM m a
getFormat format = EvalM $ gets (format . evalConfigFormat . stateConfig)

reportItem :: Monad m => Path -> Format.Item -> EvalM m ()
reportItem path item = do
  case Format.itemResult item of
    Format.Success {} -> increaseSuccessCount
    Format.Pending {} -> increasePendingCount
    Format.Failure {} -> addFailure path
  format <- getFormat formatItem
  lift (format path item)

failureItem :: Maybe Location -> Seconds -> String -> FailureReason -> Format.Item
failureItem loc duration info err = Format.Item loc duration info (Format.Failure err)

reportResult :: Monad m => Path -> Maybe Location -> (Seconds, Result) -> EvalM m ()
reportResult path loc (duration, result) = do
  case result of
    Result info status -> case status of
      Success -> reportItem path (Format.Item loc duration info Format.Success)
      Pending loc_ reason -> reportItem path (Format.Item (loc_ <|> loc) duration info $ Format.Pending reason)
      Failure loc_ err@(Error _ e) -> reportItem path (failureItem (loc_ <|> extractLocation e <|> loc) duration info err)
      Failure loc_ err -> reportItem path (failureItem (loc_ <|> loc) duration info err)

groupStarted :: Monad m => Path -> EvalM m ()
groupStarted path = do
  format <- getFormat formatGroupStarted
  lift $ format path

groupDone :: Monad m => Path -> EvalM m ()
groupDone path = do
  format <- getFormat formatGroupDone
  lift $ format path

data EvalItem a = EvalItem {
  evalItemDescription :: String
, evalItemLocation :: Maybe Location
, evalItemParallelize :: Bool
, evalItemAction :: LifeCycleCallback -> IO a
}

type EvalTree = Tree (IO ()) (EvalItem Result)
type ReportProgress m = Path -> LifeCycle Progress -> m ()

runEvalM :: Monad m => EvalConfig m -> EvalM m () -> m (State m)
runEvalM config (EvalM action) = execStateT action (State config 0 0 [])

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: forall m. MonadIO m => EvalConfig m -> [EvalTree] -> IO (Int, [Path])
runFormatter config specs = do
  atomicSemaphore <- if formatAsynchronously (evalConfigFormat config)
                     then Just <$> newSem 1
                     else return Nothing
  let
    start :: IO [RunningTree_ m Result]
    start = parallelizeTree atomicSemaphore (evalConfigConcurrentJobs config) specs
    cancel = cancelMany . concatMap toList . map (fmap fst)
  E.bracket start cancel $ \ runningSpecs -> do
    withTimer 0.05 $ \ timer -> do
      state <- formatRun format $ do
        runEvalM config $ run (reportProgress timer) (fromMaybe mempty atomicSemaphore) $
          (fmap.fmap) ((fmap.fmap.first) maybeToList . snd) runningSpecs
      let
        failures = stateFailures state
        total = stateSuccessCount state + statePendingCount state + length failures
      return (total, reverse failures)
  where
    format = evalConfigFormat config

    reportProgress :: IO Bool -> ReportProgress m
    reportProgress _timer path (Started p) =
      formatProgress format path (Started p)
    reportProgress timer path (Progress progress) = do
      r <- liftIO timer
      when r (formatProgress format path $ Progress progress)

cancelMany :: [Async a] -> IO ()
cancelMany asyncs = do
  mapM_ (killThread . asyncThreadId) asyncs
  mapM_ waitCatch asyncs

data Item a = Item {
  _itemDescription :: String
, _itemLocation :: Maybe Location
, _itemAction :: a
} deriving Functor

type Job m a = (LifeCycle Progress -> m ()) -> m a

type RunningItem m = Item (AsyncCell m (JobStatus Progress (Seconds, Result)))
type RunningTree m = Tree (IO ()) (RunningItem m)

type RunningItem_ m a = (Async (), Item (AsyncCell m (JobStatus_ (Seconds, a))))
type RunningTree_ m a = Tree (IO ()) (RunningItem_ m a)

type JobStatus    p a = Parallel [LifeCycle p] a
type JobStatus_     a = Parallel (Maybe (LifeCycle Progress)) a

parallelizeTree
  :: (MonadIO m, Traversable t)
  => Maybe Semaphore -> Int -> [t (EvalItem a)] -> IO [t (RunningItem_ m a)]
parallelizeTree atomicSem n specs = do
  sequentialSem <- newSem 1
  parallelSem   <- if n == 1 then return sequentialSem else  newSem n
  (traverse.traverse) (parallelizeItem parallelSem sequentialSem) specs
  where
  parallelizeItem parallelSem sequentialSem EvalItem{..} = do
    (asyncAction, evalAction)  <-
      parallelize parallelSem sequentialSem evalItemParallelize (interruptible . evalItemAction)
    return (asyncAction, Item evalItemDescription evalItemLocation evalAction)

  parallelize parallelSem sequentialSem isParallelizable
    | Just atomicS <- atomicSem, isParallelizable
    = runParallel parallelSem atomicS
    | Just atomicS <- atomicSem
    = runParallel sequentialSem atomicS
    | isParallelizable
    = runParallel parallelSem mempty
    | otherwise
    = runSequentially   mempty      mempty

-- | Like 'runParallel', but delays the start of the job until progress is checked
runSequentially
  :: forall m a.
     (MonadIO m)
  => Semaphore -- ^ synchronization of worker thread
  -> Semaphore -- ^ synchronization of start and done progress reports
  -> Job IO a
  -> IO (Async (), AsyncCell m (JobStatus_ (Seconds, a)))
runSequentially workSem progressSem action = do
  delaySem <- newSem 1
  semaphoreWait delaySem
  (cancel, eval) <- runParallel (delaySem M.<> workSem) progressSem action
  return (cancel, liftIO(semaphoreSignal delaySem) >> eval)

runParallel
  :: forall m a.
     (MonadIO m)
  => Semaphore -- ^ synchronization of worker thread
  -> Semaphore -- ^ synchronization of start and done progress reports
  -> Job IO a
  -> IO (Async (), AsyncCell m (JobStatus_ (Seconds, a)))
runParallel workSem progressSem action = do
  progressVar <- newEmptyMVar
  asyncAction <- async $ withSemaphore workSem (worker progressVar)
  let eval = asyncCellFromMVar progressVar
  return (asyncAction, (fmap.fmap) S.getFirst eval)
  where
    worker mvar = do
      let partialCallback x = withSemaphore progressSem $ replaceMVar mvar (\s -> fromMaybe (Partial Nothing) s S.<> Partial (Just x))
      partialCallback (Started (0,0))
      result <- measure $ action partialCallback
      replaceMVar mvar (\s -> fromMaybe (Partial Nothing) s S.<> Return Nothing (S.First result))

replaceMVar :: MVar a -> (Maybe a -> a) -> IO ()
replaceMVar mvar f = tryTakeMVar mvar >>= putMVar mvar . f

run :: forall m. MonadIO m => ReportProgress m -> Semaphore -> [RunningTree m] -> EvalM m ()
run reportProgress atomicSemaphore specs = do
  fastFail <- EvalM $ gets (evalConfigFastFail . stateConfig)
  asyncReport <- EvalM $ gets (formatAsynchronously . evalConfigFormat . stateConfig)
  flatSpecs <- lift $ concat <$> mapM foldSpec specs
  let sync = if asyncReport then Just (10, atomicSemaphore) else Nothing
  sequenceActions sync fastFail ((fmap . fmap . first) (foldMap sequence_) flatSpecs)
  where
    foldSpec :: RunningTree m -> m [AsyncCell (EvalM m) (JobStatus (EvalM m ()) (EvalM m ()))]
    foldSpec = foldTree FoldTree {
      -- Notify groupStarted as a side effect of starting children 0
      onNode = runNode
    , onNodeWithCleanup = runCleanup
    , onLeafe = (return.) . evalItem
    }

    runNode path (concat -> children) = do
        let initial = whenAnyStarted children (groupStarted path)
        children' <- whenAllReturn  children (groupDone path)
        return $ initial : children'

    runCleanup groups (concat -> children) cleanUp = do
      let cleanUp' = do
              (dt, r) <- liftIO $ measure $ safeTry cleanUp
              either (\ e -> reportItem path . failureItem (extractLocation e) dt "" . Error Nothing $ e) return r
      whenAllReturn children cleanUp'
      where
        path = (groups, "afterAll-hook")

    evalItem :: [String] -> RunningItem m -> [AsyncCell (EvalM m) (JobStatus (EvalM m ()) (EvalM m ()))]
    evalItem groups (Item requirement loc action) = [do
      res <- asyncCellHoist lift action
      return $ bimap (reportLifeCycle path <$>) (reportResult path loc) res]
      where
        path :: Path
        path = (groups, requirement)

        reportLifeCycle thePath (Started p)  =
          Started  $ lift $ reportProgress thePath (Started  p)
        reportLifeCycle thePath (Progress p) =
          Progress $ lift $ reportProgress thePath (Progress p)

    whenAnyStarted children sideEffect = unsafeAsyncCell (return $ Return mempty sideEffect) $ Just <$> do
      states <- catMaybes <$> mapM asyncCellTryRead children
      return $ if any isStarted states then Return mempty sideEffect else Partial mempty

    whenAllReturn children sideEffect = do
      barriers <- liftIO $ mapM (const newEmptyMVar) children
      let wrap c barrier = do
            x <- c
            when (isReturn x) $ void $ liftIO $ tryPutMVar barrier ()
            return x
          final = do
            mapM_ asyncCellFromMVarAlwaysRead barriers
            return $ Return mempty sideEffect
      return $ zipWith wrap children barriers ++ [final]

data FoldTree c a m r = FoldTree {
  onNode :: Path -> [r] -> m r
, onNodeWithCleanup :: [String] -> [r] -> c -> m r
, onLeafe :: [String] -> a -> m r
}

foldTree :: Monad m => FoldTree c a m r -> Tree c a -> m r
foldTree FoldTree{..} = go []
  where
    go rGroups (Node group xs) = onNode path =<< children
      where
        path = (reverse rGroups, group)
        children = mapM (go (group : rGroups)) xs
    go rGroups (NodeWithCleanup action xs) = do
      cc <- children
      onNodeWithCleanup (reverse rGroups) cc action
      where
        children = mapM (go rGroups) xs
    go rGroups (Leaf a) = onLeafe (reverse rGroups) a

sequenceActions
  :: (MonadIO m)
  => Maybe (Int, Semaphore) -- ^ Polling period in microseconds and semaphore to synchronize progress reporting. If 'Nothing' then reporting will be synchronous
  -> Bool                   -- ^ fail fast
  -> [AsyncCell (EvalM m) (Parallel (EvalM m ()) (EvalM m ()))]
  -> EvalM m ()
sequenceActions _ _ [] = return ()
-- Synchronous reporting
sequenceActions Nothing fastFail aa = go aa
  where
    go [] = return ()
    go (action : actions) = do
      status <- asyncCellTake action
      case status of
        Partial p -> p >> go (action:actions)
        Return p report -> do
          () <- p
          () <- report
          hasFailures <- EvalM $ (not . null) <$> gets stateFailures
          let stopNow = fastFail && hasFailures
          unless stopNow (go actions)
-- Asynchronous reporting
sequenceActions (Just (period, atomicSemaphore)) fastFail aa = do
    liftIO (semaphoreWait atomicSemaphore)
    go [] aa
  where
    go acc [] = do
      liftIO (semaphoreSignal atomicSemaphore *> threadDelay period)
      sequenceActions (Just (period, atomicSemaphore)) fastFail (reverse acc)
    go acc (action:actions) = do
      status <- asyncCellTryTake action
      case status of
        Nothing -> go (action:acc) actions
        Just (Partial p) -> do
          p
          go (action : acc) actions
        Just (Return p report) -> do
          p
          report
          hasFailures <- EvalM $ (not . null) <$> gets stateFailures
          let stopNow = fastFail && hasFailures
          unless stopNow (go acc actions)

isStarted :: Parallel [LifeCycle a] b -> Bool
isStarted (Partial updates) = not $ null updates
isStarted Return{} = True
