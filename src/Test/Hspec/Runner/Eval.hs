{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Runner.Eval (runFormatter) where

import           Control.Applicative
import           Control.Monad
import qualified Control.Exception as E
import           Control.Concurrent
import           System.IO (Handle)

import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Util
import           Test.Hspec.Runner.Tree
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.Timer
import           Data.Time.Clock.POSIX

type EvalTree = Tree (ProgressCallback -> FormatResult -> IO (FormatM ()))

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Bool -> Handle -> Config -> Formatter -> [Tree Item] -> FormatM ()
runFormatter useColor h c formatter specs_ = do
  headerFormatter formatter
  chan <- liftIO newChan
  reportProgress <- liftIO mkReportProgress
  run chan reportProgress c formatter specs
  where
    mkReportProgress :: IO (Path -> Progress -> IO ())
    mkReportProgress
      | useColor = every 0.05 $ exampleProgress formatter h
      | otherwise = return $ \_ _ -> return ()

    specs :: [Tree (ProgressCallback -> FormatResult -> IO (FormatM ()))]
    specs = map (fmap (parallelize . unwrapItem)) specs_

    unwrapItem :: Item -> (Bool, ProgressCallback -> IO Result)
    unwrapItem (Item isParallelizable e) = (isParallelizable, e params id)
      where
        params = Params (configQuickCheckArgs c) (configSmallCheckDepth c)

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> b -> IO ()) -> IO (a -> b -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a b -> do
    r <- timer
    when r (action a b)

type FormatResult = Either E.SomeException Result -> FormatM ()

parallelize :: (Bool, ProgressCallback -> IO Result) -> ProgressCallback -> FormatResult -> IO (FormatM ())
parallelize (isParallelizable, e)
  | isParallelizable = runParallel e
  | otherwise = runSequentially e

runSequentially :: (ProgressCallback -> IO Result) -> ProgressCallback -> FormatResult -> IO (FormatM ())
runSequentially e reportProgress formatResult = return $ do
  result <- liftIO $ evalExample (e reportProgress)
  formatResult result

data Report = ReportProgress Progress | ReportResult (Either E.SomeException Result)

runParallel :: (ProgressCallback -> IO Result) -> ProgressCallback -> FormatResult -> IO (FormatM ())
runParallel e reportProgress formatResult = do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
    let progressCallback = replaceMVar mvar . ReportProgress
    result <- evalExample (e progressCallback)
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

evalExample :: IO Result -> IO (Either E.SomeException Result)
evalExample e = safeTry $ forceResult <$> e

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
      either (failed path . Left) return r

    queueSpec :: [String] -> EvalTree -> IO ()
    queueSpec rGroups (Node group xs) = do
      defer (exampleGroupStarted formatter (reverse rGroups) group)
      forM_ xs (queueSpec (group : rGroups))
      defer (exampleGroupDone formatter)
    queueSpec rGroups (NodeWithCleanup action xs) = do
      forM_ xs (queueSpec rGroups)
      defer (runCleanup action (reverse rGroups, "afterAll-hook"))
    queueSpec rGroups (Leaf requirement e) =
      queueExample (reverse rGroups, requirement) e

    queueExample :: Path -> (ProgressCallback -> FormatResult -> IO (FormatM ())) -> IO ()
    queueExample path e = e reportProgress formatResult >>= defer
      where
        reportProgress = reportProgress_ path

        formatResult :: Either E.SomeException Result -> FormatM ()
        formatResult result = do
          case result of
            Right Success -> do
              increaseSuccessCount
              exampleSucceeded formatter path
            Right (Pending reason) -> do
              increasePendingCount
              examplePending formatter path reason
            Right (Fail err) -> failed path (Right err)
            Left err         -> failed path (Left  err)

    failed path err = do
      increaseFailCount
      addFailMessage path err
      exampleFailed formatter path err

processMessages :: IO Message -> Bool -> FormatM ()
processMessages getMessage fastFail = go
  where
    go = liftIO getMessage >>= \m -> case m of
      Run action -> do
        action
        fails <- getFailCount
        unless (fastFail && fails /= 0) go
      Done -> return ()
