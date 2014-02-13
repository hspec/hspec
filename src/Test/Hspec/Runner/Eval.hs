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

type ProgressCallback = Progress -> IO ()

run :: Chan Message -> Bool -> Handle -> Config -> Formatter -> [SpecTree] -> FormatM ()
run chan useColor h c formatter specs = do
  liftIO $ do
    forM_ (zip [0..] specs) (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan) (configFastFail c)
  where
    defer = writeChan chan . Run

    queueSpec :: [String] -> (Int, SpecTree) -> IO ()
    queueSpec rGroups (n, SpecGroup group xs) = do
      defer (exampleGroupStarted formatter n (reverse rGroups) group)
      forM_ (zip [0..] xs) (queueSpec (group : rGroups))
      defer (exampleGroupDone formatter)
    queueSpec rGroups (_, SpecItem (Item isParallelizable requirement e)) =
      queueExample isParallelizable (reverse rGroups, requirement) (`e` id)

    queueExample :: Bool -> Path -> (Params -> IO Result) -> IO ()
    queueExample isParallelizable path e
      | isParallelizable = runParallel
      | otherwise = defer runSequentially
      where
        runSequentially :: FormatM ()
        runSequentially = do
          result <- liftIO $ do
            progressCallback <- mkReportProgress
            evalExample e progressCallback
          formatResult result

        runParallel = do
          mvar <- newEmptyMVar
          _ <- forkIO $ do
            let progressCallback = replaceMVar mvar . ReportProgress
            result <- evalExample e progressCallback
            replaceMVar mvar (ReportResult result)
          reportProgress <- mkReportProgress
          defer (evalReport reportProgress mvar)
          where
            evalReport :: (Progress -> IO ()) -> MVar Report -> FormatM ()
            evalReport reportProgress mvar = do
              r <- liftIO (takeMVar mvar)
              case r of
                ReportProgress p -> do
                  liftIO $ reportProgress p
                  evalReport reportProgress mvar
                ReportResult result -> formatResult result

        mkReportProgress :: IO (Progress -> IO ())
        mkReportProgress
          | useColor = every 0.05 $ exampleProgress formatter h path
          | otherwise = return . const $ return ()

        formatResult :: Either E.SomeException Result -> FormatM ()
        formatResult result = do
          case result of
            Right Success -> do
              increaseSuccessCount
              exampleSucceeded formatter path
            Right (Pending reason) -> do
              increasePendingCount
              examplePending formatter path reason
            Right (Fail err) -> failed (Right err)
            Left err         -> failed (Left  err)
          where
            failed err = do
              increaseFailCount
              addFailMessage path err
              exampleFailed formatter path err

    evalExample :: (Params -> IO Result) -> ProgressCallback -> IO (Either E.SomeException Result)
    evalExample e progressCallback = safeTry . fmap forceResult $ e params
      where
        params = Params (configQuickCheckArgs c) (configSmallCheckDepth c) progressCallback

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p

processMessages :: IO Message -> Bool -> FormatM ()
processMessages getMessage fastFail = go
  where
    go = liftIO getMessage >>= \m -> case m of
      Run action -> do
        action
        fails <- getFailCount
        unless (fastFail && fails /= 0) go
      Done -> return ()

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> IO ()) -> IO (a -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a -> do
    r <- timer
    when r (action a)
