{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.Eval (EvalConfig(..), runFormatter) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Data.Maybe

import           Control.Monad (unless, when)
import qualified Control.Exception as E
import           Control.Concurrent

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX

import           Control.Monad.Trans.State hiding (State, state)
import           Control.Monad.Trans.Class

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Formatters hiding (FormatM)
import qualified Test.Hspec.Core.Formatters as Formatters (FormatM)
import           Test.Hspec.Core.Formatters.Internal (FormatConfig(..), FormatM, finally_)
import qualified Test.Hspec.Core.Formatters.Internal as Formatter
import           Test.Hspec.Core.Timer

data EvalConfig = EvalConfig {
  evalConfigFormat :: Formatter
, evalConfigFormatConfig :: FormatConfig
, evalConfigConcurrentJobs :: Int
, evalConfigParams :: Params
, evalConfigFastFail :: Bool
}

data State = State {
  stateFormatter :: Formatter
, stateSuccessCount :: Int
, statePendingCount :: Int
, stateFailures :: [Path]
}

type EvalM = StateT State FormatM

increaseSuccessCount :: EvalM ()
increaseSuccessCount = modify $ \state -> state {stateSuccessCount = stateSuccessCount state + 1}

increasePendingCount :: EvalM ()
increasePendingCount = modify $ \state -> state {statePendingCount = statePendingCount state + 1}

addFailure :: Path -> EvalM ()
addFailure path = modify $ \state -> state {stateFailures = path : stateFailures state}

interpret :: Formatters.FormatM a -> EvalM a
interpret = lift . Formatter.interpret

reportSuccess :: Path -> EvalM ()
reportSuccess path = do
  increaseSuccessCount
  lift Formatter.increaseSuccessCount
  format <- gets (exampleSucceeded . stateFormatter)
  interpret (format path)

reportPending :: Path -> Maybe String -> EvalM ()
reportPending path reason = do
  increasePendingCount
  lift $ Formatter.increasePendingCount
  format <- gets (examplePending . stateFormatter)
  interpret (format path reason)

reportFailure :: Maybe Location -> Path -> Either E.SomeException FailureReason -> EvalM ()
reportFailure loc path err = do
  addFailure path
  lift $ Formatter.addFailMessage loc path err
  format <- gets (exampleFailed . stateFormatter)
  interpret $ format path err

groupStarted :: [String] -> String -> EvalM ()
groupStarted nesting name = do
  format <- gets (exampleGroupStarted . stateFormatter)
  interpret $ format nesting name

groupDone :: EvalM ()
groupDone = do
  format <- gets (exampleGroupDone . stateFormatter)
  interpret format

type EvalTree = Tree (ActionWith ()) (String, Maybe Location, ProgressCallback -> FormatResult -> IO (EvalM ()))

runEvalM :: Formatter -> EvalM () -> FormatM State
runEvalM formatter action = execStateT action (State formatter 0 0 [])

runFormatM :: Formatter -> FormatConfig -> FormatM a -> IO a
runFormatM formatter config action = do
  Formatter.runFormatM config $ do
    Formatter.interpret (headerFormatter formatter)
    a <- action `finally_` Formatter.interpret (failedFormatter formatter)
    Formatter.interpret (footerFormatter formatter)
    return a

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: EvalConfig -> [SpecTree ()] -> IO (Int, [Path])
runFormatter config specs = do
  jobsSem <- newQSem (evalConfigConcurrentJobs config)
  state <- runFormatM formatter (evalConfigFormatConfig config) $ do
    runEvalM formatter (runFormatter_ config jobsSem specs)
  let
    failures = stateFailures state
    total = stateSuccessCount state + statePendingCount state + length failures
  return (total, reverse failures)
  where
    formatter = evalConfigFormat config

runFormatter_ :: EvalConfig -> QSem -> [SpecTree ()] -> EvalM ()
runFormatter_ config jobsSem specs = do
  chan <- liftIO newChan
  reportProgress <- liftIO mkReportProgress
  run chan reportProgress (evalConfigFastFail config) (toEvalTree specs)
  where
    formatter = evalConfigFormat config
    useColor = (formatConfigUseColor . evalConfigFormatConfig) config
    h = (formatConfigHandle . evalConfigFormatConfig) config

    mkReportProgress :: IO (Path -> Progress -> IO ())
    mkReportProgress
      | useColor = every 0.05 $ exampleProgress formatter h
      | otherwise = return $ \_ _ -> return ()

    toEvalTree :: [SpecTree ()] -> [EvalTree]
    toEvalTree = map (fmap f)
      where
        f :: Item () -> (String, Maybe Location, ProgressCallback -> FormatResult -> IO (EvalM ()))
        f (Item requirement loc isParallelizable e) = (requirement, loc, parallelize jobsSem isParallelizable $ e params ($ ()))

    params :: Params
    params = evalConfigParams config

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> b -> IO ()) -> IO (a -> b -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a b -> do
    r <- timer
    when r (action a b)

type FormatResult = Either E.SomeException Result -> EvalM ()

parallelize :: QSem -> Maybe Bool -> (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (EvalM ())
parallelize jobsSem isParallelizable e
  | fromMaybe False isParallelizable = runParallel jobsSem e
  | otherwise = runSequentially e

runSequentially :: (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (EvalM ())
runSequentially e reportProgress formatResult = return $ do
  result <- liftIO $ e reportProgress
  formatResult result

data Report = ReportProgress Progress | ReportResult (Either E.SomeException Result)

runParallel :: QSem -> (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (EvalM ())
runParallel jobsSem e reportProgress formatResult = do
  mvar <- newEmptyMVar
  _ <- forkIO $ E.bracket_ (waitQSem jobsSem) (signalQSem jobsSem) $ do
    let progressCallback = replaceMVar mvar . ReportProgress
    result <- e progressCallback
    replaceMVar mvar (ReportResult result)
  return $ evalReport mvar
  where
    evalReport :: MVar Report -> EvalM ()
    evalReport mvar = do
      r <- liftIO (takeMVar mvar)
      case r of
        ReportProgress p -> do
          liftIO $ reportProgress p
          evalReport mvar
        ReportResult result -> formatResult result

    replaceMVar :: MVar a -> a -> IO ()
    replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

data Message = Done | Run (EvalM ())

run :: Chan Message -> (Path -> ProgressCallback) -> Bool -> [EvalTree] -> EvalM ()
run chan reportProgress_ fastFail specs = do
  liftIO $ do
    forM_ specs (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan) fastFail
  where
    defer :: EvalM () -> IO ()
    defer = writeChan chan . Run

    runCleanup :: IO () -> Path -> EvalM ()
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
      defer (runCleanup (action ()) (reverse rGroups, "afterAll-hook"))
    queueSpec rGroups (Leaf e) =
      queueExample (reverse rGroups) e

    queueExample :: [String] -> (String, Maybe Location, ProgressCallback -> FormatResult -> IO (EvalM ())) -> IO ()
    queueExample groups (requirement, loc, e) = e reportProgress reportResult >>= defer
      where
        path :: Path
        path = (groups, requirement)

        reportProgress = reportProgress_ path

        reportResult :: FormatResult
        reportResult result = do
          case result of
            Right Success -> reportSuccess path
            Right (Pending reason) -> reportPending path reason
            Right (Failure loc_ err) -> reportFailure (loc_ <|> loc) path (Right err)
            Left err -> reportFailure loc path (Left  err)

processMessages :: IO Message -> Bool -> EvalM ()
processMessages getMessage fastFail = go
  where
    go = liftIO getMessage >>= \m -> case m of
      Run action -> do
        action
        hasFailures <- (not . null) <$> gets stateFailures
        let stopNow = fastFail && hasFailures
        unless stopNow go
      Done -> return ()
