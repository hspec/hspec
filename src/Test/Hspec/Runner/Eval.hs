{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Runner.Eval (runFormatter) where

import           Control.Applicative
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

data Tree a
  = Node !String [Tree a]
  | Leaf !String a
  deriving (Eq, Show, Functor)

toTree :: SpecTree -> Tree Item
toTree spec = case spec of
  SpecGroup label specs -> Node label (map toTree specs)
  SpecItem r item -> Leaf r item

type EvalTree = Tree (ProgressCallback -> FormatResult -> IO (FormatM ()))

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Bool -> Handle -> Config -> Formatter -> [SpecTree] -> FormatM ()
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

    specs = map (fmap (parallelize . fmap (applyNoOpAround . applyQuickCheckArgs) . unwrapItem) . toTree) specs_

    unwrapItem :: Item -> (Bool, Params -> (IO () -> IO ()) -> IO Result)
    unwrapItem (Item isParallelizable e) = (isParallelizable, e)

    applyQuickCheckArgs :: (Params -> a) -> ProgressCallback -> a
    applyQuickCheckArgs e progressCallback = e $ Params (configQuickCheckArgs c) (configSmallCheckDepth c) progressCallback

    applyNoOpAround :: (a -> (IO () -> IO ()) -> b) -> a -> b
    applyNoOpAround = fmap ($ id)

-- | Execute given action at most every specified number of seconds.
every :: POSIXTime -> (a -> b -> IO ()) -> IO (a -> b -> IO ())
every seconds action = do
  timer <- newTimer seconds
  return $ \a b -> do
    r <- timer
    when r (action a b)

type ProgressCallback = Progress -> IO ()
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
    forM_ (zip [0..] specs) (queueSpec [])
    writeChan chan Done
  processMessages (readChan chan) (configFastFail c)
  where
    defer :: FormatM () -> IO ()
    defer = writeChan chan . Run

    queueSpec :: [String] -> (Int, EvalTree) -> IO ()
    queueSpec rGroups (n, Node group xs) = do
      defer (exampleGroupStarted formatter n (reverse rGroups) group)
      forM_ (zip [0..] xs) (queueSpec (group : rGroups))
      defer (exampleGroupDone formatter)
    queueSpec rGroups (_, Leaf requirement e) =
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
            Right (Fail err) -> failed (Right err)
            Left err         -> failed (Left  err)
          where
            failed err = do
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
