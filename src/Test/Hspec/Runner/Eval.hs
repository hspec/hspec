module Test.Hspec.Runner.Eval (runFormatter) where

import           Control.Monad
import qualified Control.Exception as E
import           Control.Concurrent
import           Data.Either (rights)
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

data CloseAction = CloseAction Int (IO ()) -- ^ remaining unevalExampled items and the close action

-- | Decrement the count.  If the count reaches zero, evalExample the action.
cleanup :: MVar CloseAction -> IO ()
cleanup mv = modifyMVar_ mv $ \(CloseAction n a) -> do
    if n <= 1
        then safeTry a >> return (CloseAction 0 (return ()))
        else return $ CloseAction (n-1) a

-- | A bracket action to evalExample before running an item.  There is exactly one of these
-- created for each 'SpecBracket' and it is stored in an MVar.
data BracketAction = AlreadyOpen (Either E.SomeException (SomeParam, IO ()))
                      -- ^               open error       param      action to run on finish
                   | DelayedOpen (IO SomeParam) (SomeParam -> IO ()) Int
                      -- ^        open action    close action        number of spec items

-- | Open the bracket action.  Either returns an error thrown by the create function or
-- the actual param and an action to be used for cleanup.
openParam :: MVar BracketAction -> IO (Either E.SomeException (SomeParam, IO ()))
openParam mv = modifyMVar mv $ \o ->
  case o of
    AlreadyOpen p -> return (o, p)
    DelayedOpen open close n -> do
      mp <- safeTry open
      case mp of
        Left err -> return (AlreadyOpen $ Left err, Left err)
        Right p -> do
            closevar <- newMVar $ CloseAction n $ close p
            let res = Right (p, cleanup closevar)
            return (AlreadyOpen res, res)

-- | Execute a single item, opening all bracketed actions in the list, running the actual item,
-- and then running the cleanup actions.
evalExample :: Config -> ([SomeParam] -> IO Result) -> (Progress -> IO ())
        -> [MVar BracketAction] -> IO (Either E.SomeException Result)
evalExample c e pg open
  | configDryRun c = return $ Right Success
  | otherwise = do
      params <- mapM openParam open
      let actions = sequence params -- sequence in the Either monad
      case actions of
        Left err -> do
          sequence_ $ map snd $ rights params -- close everything
          return $ Left err

        Right lst -> do
          let def = [ SomeParam $ QuickCheckArgs $ configQuickCheckArgs c
                    , SomeParam $ SmallCheckParams $ configSmallCheckDepth c
                    , SomeParam $ ReportProgressParam pg
                    ]
          result <- safeTry $ fmap forceResult $ e $ def ++ map fst lst
          sequence_ $ map snd $ rights params -- close everything
          return result

run :: Chan Message -> Bool -> Handle -> Config -> Formatter -> [SpecTree] -> FormatM ()
run chan useColor h c formatter specs = do
  liftIO $ do
    forM_ (zip [0..] specs) (queueSpec [] [])
    writeChan chan Done
  processChan chan (configFastFail c)
  where
    defer = writeChan chan . Run

    queueSpec :: [String] -> [MVar BracketAction] -> (Int, SpecTree) -> IO ()
    queueSpec rGroups bracketAct (n, SpecGroup group xs) = do
      defer (exampleGroupStarted formatter n (reverse rGroups) group)
      forM_ (zip [0..] xs) (queueSpec (group : rGroups) bracketAct)
      defer (exampleGroupDone formatter)

    queueSpec rGroups bracketAct (_, SpecItem (Item isParallelizable requirement e)) =
      queueExample isParallelizable (reverse rGroups, requirement) bracketAct (`e` id)

    queueSpec rGroups bracketAct (n, SpecBracket create close spec) = do
      mopen <- newMVar $ DelayedOpen create close $ countItems spec
      queueSpec rGroups (mopen:bracketAct) (n, spec)

    countItems :: SpecTree -> Int
    countItems (SpecGroup _ xs) = sum $ map countItems xs
    countItems (SpecBracket _ _ s) = countItems s
    countItems (SpecItem (Item _ _ _)) = 1

    queueExample :: Bool -> Path -> [MVar BracketAction] -> ([SomeParam] -> IO Result) -> IO ()
    queueExample isParallelizable path bracketAct e
      | isParallelizable = runParallel
      | otherwise = defer runSequentially
      where
        runSequentially :: FormatM ()
        runSequentially = do
          progressHandler <- liftIO (mkProgressHandler reportProgress)
          result <- liftIO $ evalExample c e progressHandler $ reverse bracketAct
          formatResult formatter path result

        runParallel = do
          mvar <- newEmptyMVar
          _ <- forkIO $ do
            progressHandler <- mkProgressHandler (replaceMVar mvar . ReportProgress)
            result <- evalExample c e progressHandler $ reverse bracketAct
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
