{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.Core.Runner.Eval (
  EvalConfig(..)
, ColorMode(..)
, EvalTree
, Tree(..)
, EvalItem(..)
, Concurrency(..)
, runFormatter
#ifdef TEST
, mergeResults
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (Monad)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Progress, FailureReason(..), Result(..), ResultStatus(..), ProgressCallback)
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format (Format)
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example.Location
import           Test.Hspec.Core.Example (safeEvaluateResultStatus, exceptionToResultStatus)

import qualified NonEmpty
import           NonEmpty (NonEmpty(..))

import           Test.Hspec.Core.Runner.JobQueue

data Tree c a =
    Node String (NonEmpty (Tree c a))
  | NodeWithCleanup (Maybe (String, Location)) c (NonEmpty (Tree c a))
  | Leaf a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data EvalConfig = EvalConfig {
  evalConfigFormat :: Format
, evalConfigConcurrentJobs :: Int
, evalConfigFailFast :: Bool
, evalConfigColorMode :: ColorMode
}

data ColorMode = ColorDisabled | ColorEnabled

data Env = Env {
  envConfig :: EvalConfig
, envFailed :: IORef Bool
, envResults :: IORef [(Path, Format.Item)]
}

formatEvent :: Format.Event -> EvalM ()
formatEvent event = do
  format <- asks $ evalConfigFormat . envConfig
  liftIO $ format event

type EvalM = ReaderT Env IO

setFailed :: EvalM ()
setFailed = do
  ref <- asks envFailed
  liftIO $ writeIORef ref True

hasFailed :: EvalM Bool
hasFailed = do
  ref <- asks envFailed
  liftIO $ readIORef ref

addResult :: Path -> Format.Item -> EvalM ()
addResult path item = do
  ref <- asks envResults
  liftIO $ modifyIORef ref ((path, item) :)

reportItem :: Path -> Maybe Location -> EvalM (Seconds, Result)  -> EvalM ()
reportItem path loc action = do
  reportItemStarted path
  action >>= reportResult path loc

reportItemStarted :: Path -> EvalM ()
reportItemStarted = formatEvent . Format.ItemStarted

reportItemDone :: Path -> Format.Item -> EvalM ()
reportItemDone path item = do
  let
    isFailure = case Format.itemResult item of
      Format.Success{} -> False
      Format.Pending{} -> False
      Format.Failure{} -> True
  when isFailure setFailed
  addResult path item
  formatEvent $ Format.ItemDone path item

reportResult :: Path -> Maybe Location -> (Seconds, Result) -> EvalM ()
reportResult path loc (duration, result) = do
  mode <- asks (evalConfigColorMode . envConfig)
  case result of
    Result info status -> reportItemDone path $ Format.Item loc duration info $ case status of
      Success                      -> Format.Success
      Pending loc_ reason          -> Format.Pending loc_ reason
      Failure loc_ err@(Error _ e) -> Format.Failure (loc_ <|> extractLocation e) err
      Failure loc_ err             -> Format.Failure loc_ $ case mode of
        ColorEnabled -> err
        ColorDisabled -> case err of
          NoReason -> err
          Reason _ -> err
          ExpectedButGot _ _ _ -> err
          ColorizedReason r -> Reason (stripAnsi r)
#if __GLASGOW_HASKELL__ < 900
          Error _ _ -> err
#endif

groupStarted :: Path -> EvalM ()
groupStarted = formatEvent . Format.GroupStarted

groupDone :: Path -> EvalM ()
groupDone = formatEvent . Format.GroupDone

data EvalItem = EvalItem {
  evalItemDescription :: String
, evalItemLocation :: Maybe Location
, evalItemConcurrency :: Concurrency
, evalItemAction :: ProgressCallback -> IO (Seconds, Result)
}

type EvalTree = Tree (IO ()) EvalItem

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: EvalConfig -> [EvalTree] -> IO [(Path, Format.Item)]
runFormatter config specs = do
  withJobQueue (evalConfigConcurrentJobs config) $ \ queue -> do
    withTimer 0.05 $ \ timer -> do
      env <- mkEnv
      runningSpecs_ <- enqueueItems queue specs

      let
        applyReportProgress :: RunningItem_ IO -> RunningItem
        applyReportProgress item = fmap (. reportProgress timer) item

        runningSpecs :: [RunningTree ()]
        runningSpecs = applyCleanup $ map (fmap applyReportProgress) runningSpecs_

        getResults :: IO [(Path, Format.Item)]
        getResults = reverse <$> readIORef (envResults env)

        formatItems :: IO ()
        formatItems = runReaderT (eval runningSpecs) env

        formatDone :: IO ()
        formatDone = getResults >>= format . Format.Done

      format Format.Started
      formatItems `finally` formatDone
      getResults
  where
    mkEnv :: IO Env
    mkEnv = Env config <$> newIORef False <*> newIORef []

    format :: Format
    format = evalConfigFormat config

    reportProgress :: IO Bool -> Path -> Progress -> IO ()
    reportProgress timer path progress = do
      r <- timer
      when r $ do
        format (Format.Progress path progress)

data Item a = Item {
  itemDescription :: String
, itemLocation :: Maybe Location
, itemAction :: a
} deriving Functor

type RunningItem = Item (Path -> IO (Seconds, Result))
type RunningTree c = Tree c RunningItem

type RunningItem_ m = Item (Job m Progress (Seconds, Result))
type RunningTree_ m = Tree (IO ()) (RunningItem_ m)

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

enqueueItems :: MonadIO m => JobQueue -> [EvalTree] -> IO [RunningTree_ m]
enqueueItems queue = mapM (traverse $ enqueueItem queue)

enqueueItem :: MonadIO m => JobQueue -> EvalItem -> IO (RunningItem_ m)
enqueueItem queue EvalItem{..} = do
  job <- enqueueJob queue evalItemConcurrency evalItemAction
  return Item {
    itemDescription = evalItemDescription
  , itemLocation = evalItemLocation
  , itemAction = job >=> liftIO . either exceptionToResult return
  }
  where
    exceptionToResult :: SomeException -> IO (Seconds, Result)
    exceptionToResult err = (,) 0 . Result "" <$> exceptionToResultStatus err

eval :: [RunningTree ()] -> EvalM ()
eval specs = do
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
        True -> hasFailed
      unless stopNow (go actions)
