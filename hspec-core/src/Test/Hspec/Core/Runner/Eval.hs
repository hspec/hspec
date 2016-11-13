{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,6,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.Eval (runFormatter) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad (unless, when)
import qualified Control.Exception as E
import           Control.Concurrent
import           System.IO (Handle)

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Formatters hiding (FormatM)
import           Test.Hspec.Core.Formatters.Internal
import qualified Test.Hspec.Core.Formatters.Internal as Formatter
import           Test.Hspec.Core.Timer

type EvalTree = Tree (ActionWith ()) (String, Maybe Location, ProgressCallback -> FormatResult -> IO (FormatM ()))

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: QSem -> Bool -> Handle -> Config -> Formatter -> [SpecTree ()] -> FormatM ()
runFormatter jobsSem useColor h c formatter specs = do
  Formatter.interpret $ headerFormatter formatter
  chan <- liftIO newChan
  reportProgress <- liftIO mkReportProgress
  run chan reportProgress c formatter (toEvalTree specs)
  where
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
    params = Params (configQuickCheckArgs c) (configSmallCheckDepth c)

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> b -> IO ()) -> IO (a -> b -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a b -> do
    r <- timer
    when r (action a b)

type FormatResult = Either E.SomeException Result -> FormatM ()

parallelize :: QSem -> Bool -> (ProgressCallback -> IO (Either E.SomeException Result)) -> ProgressCallback -> FormatResult -> IO (FormatM ())
parallelize jobsSem isParallelizable e
  | isParallelizable = runParallel jobsSem e
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

run :: Chan Message -> (Path -> ProgressCallback) -> Config -> Formatter -> [EvalTree] -> FormatM ()
run chan reportProgress_ c formatter specs = do
  liftIO $ do
    forM_ specs (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan) (configFastFail c)
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
      increaseFailCount
      addFailMessage loc path err
      Formatter.interpret $ exampleFailed formatter path err

processMessages :: IO Message -> Bool -> FormatM ()
processMessages getMessage fastFail = go
  where
    go = liftIO getMessage >>= \m -> case m of
      Run action -> do
        action
        fails <- Formatter.interpret getFailCount
        unless (fastFail && fails /= 0) go
      Done -> return ()
