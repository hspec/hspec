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

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Formatters hiding (FormatM)
import           Test.Hspec.Core.Formatters.Internal
import qualified Test.Hspec.Core.Formatters.Internal as Formatter (interpret)
import           Test.Hspec.Core.Timer

data EvalConfig = EvalConfig {
  evalConfigFormat :: Formatter
, evalConfigFormatConfig :: FormatConfig
, evalConfigConcurrentJobs :: Int
, evalConfigParams :: Params
, evalConfigFastFail :: Bool
}

type EvalTree = Tree (ActionWith ()) (String, Maybe Location, ProgressCallback -> FormatResult -> IO (FormatM ()))

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: EvalConfig -> [SpecTree ()] -> IO (Int, [Path])
runFormatter config specs = do
  jobsSem <- newQSem (evalConfigConcurrentJobs config)
  runFormatM (evalConfigFormatConfig config) $ do
    runFormatter_ config jobsSem specs `finally_` do
      Formatter.interpret $ failedFormatter formatter
    Formatter.interpret $ footerFormatter formatter
    total <- Formatter.interpret getTotalCount
    failures <- map failureRecordPath <$> Formatter.interpret getFailMessages
    return (total, failures)
  where
    formatter = evalConfigFormat config

runFormatter_ :: EvalConfig -> QSem -> [SpecTree ()] -> FormatM ()
runFormatter_ config jobsSem specs = do
  Formatter.interpret $ headerFormatter formatter
  chan <- liftIO newChan
  reportProgress <- liftIO mkReportProgress
  run chan reportProgress (evalConfigFastFail config) formatter (toEvalTree specs)
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
        f :: Item () -> (String, Maybe Location, ProgressCallback -> FormatResult -> IO (FormatM ()))
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

type FormatResult = Either E.SomeException Result -> FormatM ()

parallelize :: QSem -> Maybe Bool -> (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (FormatM ())
parallelize jobsSem isParallelizable e
  | fromMaybe False isParallelizable = runParallel jobsSem e
  | otherwise = runSequentially e

runSequentially :: (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (FormatM ())
runSequentially e reportProgress formatResult = return $ do
  result <- liftIO $ e reportProgress
  formatResult result

data Report = ReportProgress Progress | ReportResult (Either E.SomeException Result)

runParallel :: QSem -> (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (FormatM ())
runParallel jobsSem e reportProgress formatResult = do
  mvar <- newEmptyMVar
  _ <- forkIO $ E.bracket_ (waitQSem jobsSem) (signalQSem jobsSem) $ do
    let progressCallback = replaceMVar mvar . ReportProgress
    result <- e progressCallback
    replaceMVar mvar (ReportResult result)
  return $ evalReport mvar
  where
    evalReport :: MVar Report -> FormatM ()
    evalReport mvar = do
      r <- liftIO (takeMVar mvar)
      case r of
        ReportProgress p -> do
          liftIO $ reportProgress p
          evalReport mvar
        ReportResult result -> formatResult result

    replaceMVar :: MVar a -> a -> IO ()
    replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

data Message = Done | Run (FormatM ())

run :: Chan Message -> (Path -> ProgressCallback) -> Bool -> Formatter -> [EvalTree] -> FormatM ()
run chan reportProgress_ fastFail formatter specs = do
  liftIO $ do
    forM_ specs (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan) fastFail
  where
    defer :: FormatM () -> IO ()
    defer = writeChan chan . Run

    runCleanup :: IO () -> Path -> FormatM ()
    runCleanup action path = do
      r <- liftIO $ safeTry action
      either (failed Nothing path . Left) return r

    queueSpec :: [String] -> EvalTree -> IO ()
    queueSpec rGroups (Node group xs) = do
      defer (Formatter.interpret $ exampleGroupStarted formatter (reverse rGroups) group)
      forM_ xs (queueSpec (group : rGroups))
      defer (Formatter.interpret $ exampleGroupDone formatter)
    queueSpec rGroups (NodeWithCleanup action xs) = do
      forM_ xs (queueSpec rGroups)
      defer (runCleanup (action ()) (reverse rGroups, "afterAll-hook"))
    queueSpec rGroups (Leaf e) =
      queueExample (reverse rGroups) e

    queueExample :: [String] -> (String, Maybe Location, ProgressCallback -> FormatResult -> IO (FormatM ())) -> IO ()
    queueExample groups (requirement, loc, e) = e reportProgress formatResult >>= defer
      where
        path :: Path
        path = (groups, requirement)

        reportProgress = reportProgress_ path

        formatResult :: FormatResult
        formatResult result = do
          case result of
            Right Success -> do
              increaseSuccessCount
              Formatter.interpret $ exampleSucceeded formatter path
            Right (Pending reason) -> do
              increasePendingCount
              Formatter.interpret $ examplePending formatter path reason
            Right (Failure loc_ err) -> failed (loc_ <|> loc) path (Right err)
            Left err         -> failed loc path (Left  err)

    failed loc path err = do
      addFailMessage loc path err
      Formatter.interpret $ exampleFailed formatter path err

processMessages :: IO Message -> Bool -> FormatM ()
processMessages getMessage fastFail = go
  where
    go = liftIO getMessage >>= \m -> case m of
      Run action -> do
        action
        hasFailures <- (not . null) <$> Formatter.interpret getFailMessages
        let stopNow = fastFail && hasFailures
        unless stopNow go
      Done -> return ()
