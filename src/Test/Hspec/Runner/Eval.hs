module Test.Hspec.Runner.Eval (runFormatter) where

import           Control.Monad
import qualified Control.Exception as E
import           Control.Concurrent
import           System.IO (Handle)

import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Util
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.Timer
import           Data.Time.Clock.POSIX

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Bool -> Handle -> Config -> Formatter -> [SpecTree] -> FormatM ()
runFormatter useColor h c formatter specs = do
  headerFormatter formatter
  chan <- liftIO newChan
  run chan useColor h c formatter specs

data Message = Done | Run (FormatM ())

data Report = ReportProgress Progress | ReportResult (Either E.SomeException Result)

run :: Chan Message -> Bool -> Handle -> Config -> Formatter -> [SpecTree] -> FormatM ()
run chan useColor h c formatter specs = do
  liftIO $ do
    forM_ (zip [0..] specs) (queueSpec [])
    writeChan chan Done
  processChan chan (configFastFail c)
  where
    defer = writeChan chan . Run

    queueSpec :: [String] -> (Int, SpecTree) -> IO ()
    queueSpec rGroups (n, SpecGroup group xs) = do
      defer (exampleGroupStarted formatter n (reverse rGroups) group)
      forM_ (zip [0..] xs) (queueSpec (group : rGroups))
      defer (exampleGroupDone formatter)
    queueSpec rGroups (_, SpecItem isParallelizable requirement e) =
      queueExample isParallelizable (reverse rGroups, requirement) e

    queueExample :: Bool -> Path -> (Params -> IO Result) -> IO ()
    queueExample isParallelizable path e
      | isParallelizable = runParallel
      | otherwise = defer runSequentially
      where
        runSequentially :: FormatM ()
        runSequentially = do
          progressHandler <- liftIO (mkProgressHandler reportProgress)
          result <- liftIO (evalExample e progressHandler)
          formatResult formatter path result

        runParallel = do
          mvar <- newEmptyMVar
          _ <- forkIO $ do
            progressHandler <- mkProgressHandler (replaceMVar mvar . ReportProgress)
            result <- evalExample e progressHandler
            replaceMVar mvar (ReportResult result)
          defer (evalReport mvar)
          where
            evalReport :: MVar Report -> FormatM ()
            evalReport mvar = do
              r <- liftIO (takeMVar mvar)
              case r of
                ReportProgress p -> do
                  liftIO $ reportProgress p
                  evalReport mvar
                ReportResult result -> formatResult formatter path result

        reportProgress :: (Int, Int) -> IO ()
        reportProgress = exampleProgress formatter h path

    mkProgressHandler :: (a -> IO ()) -> IO (a -> IO ())
    mkProgressHandler report
      | useColor = every 0.05 report
      | otherwise = return . const $ return ()

    evalExample :: (Params -> IO Result) -> (Progress -> IO ()) -> IO (Either E.SomeException Result)
    evalExample e progressHandler
      | configDryRun c = return (Right Success)
      | otherwise      = (safeTry . fmap forceResult) (e $ Params (configQuickCheckArgs c) (configSmallCheckDepth c) progressHandler)

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

processChan :: Chan Message -> Bool -> FormatM ()
processChan chan fastFail = go
  where
    go = do
      m <- liftIO (readChan chan)
      case m of
        Run action -> do
          action
          fails <- getFailCount
          unless (fastFail && fails /= 0) go
        Done -> return ()

formatResult :: Formatter -> ([String], String) -> Either E.SomeException Result -> FormatM ()
formatResult formatter path result = do
  case result of
    Right Success -> do
      increaseSuccessCount
      exampleSucceeded formatter path
    Right (Pending reason) -> do
      increasePendingCount
      examplePending formatter path reason
    Right (Fail err) -> failed (Right err)
    Left e           -> failed (Left  e)
  where
    failed err = do
      increaseFailCount
      addFailMessage path err
      exampleFailed  formatter path err

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> IO ()) -> IO (a -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a -> do
    r <- timer
    when r (action a)
