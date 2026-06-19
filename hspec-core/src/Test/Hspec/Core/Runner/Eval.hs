{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Progress, FailureReason(..), Result(..), ResultStatus(..), ProgressCallback)
import           Test.Hspec.Core.Timer
import           Test.Hspec.Core.Format (Format)
import qualified Test.Hspec.Core.Format as Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example.Location
import           Test.Hspec.Core.Example (safeEvaluateResultStatus, exceptionToResultStatus)

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))

import           Test.Hspec.Core.Runner.JobQueue (JobQueue, Open, Concurrency(..), AbortEarly(..), Report(..))
import qualified Test.Hspec.Core.Runner.JobQueue as JobQueue

data Tree c a =
    Node !String !(NonEmpty (Tree c a))
  | NodeWithCleanup !(Maybe (String, Location)) !c !(NonEmpty (Tree c a))
  | Leaf !a
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

reportItemDone :: Path -> Format.Item -> EvalM ()
reportItemDone path item = do
  addResult path item
  formatEvent $ Format.ItemDone path item

isFailure :: Result -> Bool
isFailure r = case resultStatus r of
  Success{} -> False
  Pending{} -> False
  Failure{} -> True

formatResult :: Path -> Maybe Location -> (Seconds, Result) -> EvalM ()
formatResult path loc (duration, result) = do
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

data EvalItem = EvalItem {
  evalItemDescription :: String
, evalItemLocation :: Maybe Location
, evalItemConcurrency :: Concurrency
, evalItemAction :: ProgressCallback -> IO Result
}

type EvalTree = Tree (IO ()) EvalItem

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: EvalConfig -> [EvalTree] -> IO [(Path, Format.Item)]
runFormatter config specs = do
  queue <- JobQueue.new
  enqueuedSpecs <- enqueueItems queue specs
  jobs <- JobQueue.finalize queue
  withTimer 0.05 $ \ timer -> do
    env <- mkEnv
    let
      applyReportResult :: Item -> WithReportResult () Item
      applyReportResult =  WithReportResult formatResult

      runningSpecs :: [Tree () (WithReportResult_ Item)]
      runningSpecs = applyCleanup abortEarly $ map (fmap applyReportResult) enqueuedSpecs

      runEvalM :: EvalM a -> IO a
      runEvalM = flip runReaderT env

      eval :: forall item progress a. ([String] -> item -> [Report progress a]) -> [(Tree () item)] -> [Report progress a]
      eval evalItem = concat . concatMap foldSpec
        where
          foldSpec :: Tree () item -> [[Report progress a]]
          foldSpec = foldTree FoldTree {
            onGroupStarted = return . Report . groupStarted
          , onGroupDone = return . Report . groupDone
          , onCleanup
          , onLeaf = evalItem
          }

          groupStarted :: Path -> IO ()
          groupStarted = format . Format.GroupStarted

          groupDone :: Path -> IO ()
          groupDone = format . Format.GroupDone

          onCleanup :: Maybe (String, Location) -> [String] -> () -> [Report progress a]
          onCleanup _loc _groups () = []

      items :: [Report Progress Result]
      items = eval evalItem runningSpecs
        where
          evalItem :: [String] -> WithReportResult_ Item -> [Report Progress Result]
          evalItem groups (WithReportResult reportResult (Item requirement loc action)) = [
              Report (reportItemStarted path)
            , ReportResult action progress result
            ]
            where
              path :: Path
              path = (groups, requirement)

              progress :: ProgressCallback
              progress = reportProgress timer path

              result :: (Seconds, Either SomeException Result) -> IO AbortEarly
              result = traverse (either exceptionToResult return) >=> runEvalM . reportResult path loc

          reportItemStarted :: Path -> IO ()
          reportItemStarted = format . Format.ItemStarted

      abortEarly :: Result -> Bool
      abortEarly result = evalConfigFailFast config && isFailure result

      getResults :: IO [(Path, Format.Item)]
      getResults = reverse <$> readIORef (envResults env)

      formatItems :: IO ()
      formatItems = JobQueue.run (evalConfigConcurrentJobs config) jobs items

      formatDone :: IO ()
      formatDone = getResults >>= format . Format.Done

    format Format.Started
    formatItems `finally` formatDone
    getResults
  where
    mkEnv :: IO Env
    mkEnv = Env config <$> newIORef []

    format :: Format
    format = evalConfigFormat config

    reportProgress :: IO Bool -> Path -> Progress -> IO ()
    reportProgress timer path progress = do
      r <- timer
      when r $ do
        format (Format.Progress path progress)

type ReportResult abort = Path -> Maybe Location -> (Seconds, Result) -> EvalM abort

data WithReportResult abort a = WithReportResult {
  _reportResult :: ReportResult abort
, _item :: a
}

type WithReportResult_ = WithReportResult AbortEarly

data Item = Item {
  itemDescription :: String
, itemLocation :: Maybe Location
, itemAction :: JobQueue.Result Progress Result
}

applyFailFast :: (Result -> Bool) -> Tree c (WithReportResult () a) -> Tree c (WithReportResult_ a)
applyFailFast abortEarly = fmap applyToWithReportResult
  where
    applyToWithReportResult :: WithReportResult () a -> WithReportResult_ a
    applyToWithReportResult (WithReportResult report a) = WithReportResult (applyToReportResult report) a

    applyToReportResult :: ReportResult () -> ReportResult AbortEarly
    applyToReportResult report path loc result@(_, r) = do
      report path loc result
      return $ if abortEarly r then AbortEarly else NoAbortEarly

type Children c a = NonEmpty (Tree c (WithReportResult_ a))

applyCleanup :: (Result -> Bool) -> [Tree (IO ()) (WithReportResult () a)] -> [Tree () (WithReportResult_ a)]
applyCleanup abortEarly = map (go . applyFailFast abortEarly)
  where
    go :: Tree (IO ()) (WithReportResult_ a) -> Tree () (WithReportResult_ a)
    go t = case t of
      Node label xs -> Node label (go <$> xs)
      NodeWithCleanup loc cleanup xs -> NodeWithCleanup loc () (go <$> apply loc cleanup xs)
      Leaf a -> Leaf a

    apply :: Maybe (String, Location) -> IO () -> Children c a -> Children c a
    apply loc cleanup = forEachLeaf (addCleanupOn abortEarly) . forLastLeaf (addCleanupOn (not . abortEarly))
      where
        addCleanupOn :: (Result -> Bool) -> WithReportResult abort a -> WithReportResult abort a
        addCleanupOn p (WithReportResult report item) = WithReportResult (addCleanup p loc cleanup report) item

forEachLeaf :: (a -> b) -> NonEmpty (Tree c a) -> NonEmpty (Tree c b)
forEachLeaf f = fmap (fmap f)

forLastLeaf :: (a -> a) -> NonEmpty (Tree c a) -> NonEmpty (Tree c a)
forLastLeaf p = go
  where
    go = NonEmpty.reverse . mapHead goNode . NonEmpty.reverse

    goNode node = case node of
      Node description xs -> Node description (go xs)
      NodeWithCleanup loc_ c xs -> NodeWithCleanup loc_ c (go xs)
      Leaf item -> Leaf (p item)

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead f xs = case xs of
  y :| ys -> f y :| ys

addCleanup :: (Result -> Bool) -> Maybe (String, Location) -> IO () -> ReportResult abort -> ReportResult abort
addCleanup shouldRunCleanup loc cleanup reportProgress path l result@(t1, r1) = do
  if shouldRunCleanup r1 then do
    (t2, r2) <- liftIO $ measure $ safeEvaluateResultStatus (cleanup >> return Success)
    let t = t1 + t2
    reportProgress path l $ (t, mergeResults loc r1 r2)
  else do
    reportProgress path l result

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

enqueueItems :: JobQueue Open Progress Result -> [Tree c EvalItem] -> IO [Tree c Item]
enqueueItems queue = mapM (traverse $ enqueueItem queue)

enqueueItem :: JobQueue Open Progress Result -> EvalItem -> IO Item
enqueueItem queue EvalItem{..} = do
  job <- JobQueue.enqueue queue evalItemConcurrency evalItemAction
  return Item {
    itemDescription = evalItemDescription
  , itemLocation = evalItemLocation
  , itemAction = job
  }

exceptionToResult :: SomeException -> IO Result
exceptionToResult err = Result "" <$> exceptionToResultStatus err

data FoldTree c a r = FoldTree {
  onGroupStarted :: Path -> r
, onGroupDone :: Path -> r
, onCleanup :: Maybe (String, Location) -> [String] -> c -> r
, onLeaf :: [String] -> a -> r
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
    go rGroups (Leaf a) = [onLeaf (reverse rGroups) a]
