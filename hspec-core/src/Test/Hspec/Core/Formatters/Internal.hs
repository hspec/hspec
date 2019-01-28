{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Test.Hspec.Core.Formatters.Internal (
  FormatConfig(..)
, runFormatM
, interpret
, increaseSuccessCount
, increasePendingCount
, addFailMessage
, finally_
, formatterToFormat
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified System.IO as IO
import           Control.Exception (AsyncException(..))
import           Control.Monad.Catch(throwM, try, MonadMask)
import           System.Console.ANSI
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State hiding (gets, modify)
import           Data.Char (isSpace)
import           GHC.IO.Handle
import qualified System.CPUTime as CPUTime
import           System.IO.Silently

import           Test.Hspec.Core.Formatters.Free
import qualified Test.Hspec.Core.Formatters.Monad as M
import           Test.Hspec.Core.Formatters.Monad (Environment(..), interpretWith, FailureRecord(..))
import           Test.Hspec.Core.Formatters.TextBlock
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Spec (LifeCycle(..))

formatterToFormat
  :: M.Formatter
  -> FormatConfig
  -> Format FormatM
formatterToFormat formatter config = Format {
  formatRun = \action -> do
      let runStuff h =
            runTextIO h config $
            runFormatM config $ do
              interpret (M.headerFormatter formatter)
              a <- action `finally_` interpret (M.failedFormatter formatter)
              interpret (M.footerFormatter formatter)
              return a
      case M.capturedOutputFormatter formatter of
        Nothing -> runStuff (formatConfigHandle config)
        Just capturedF -> do
          h' <- hDuplicate (formatConfigHandle config)
          (output, res) <- hCapture [IO.stdout, IO.stderr] $ runStuff h'
          runTextIO h' config $ runFormatM config $ interpret (capturedF output)
          hClose h'
          return res

, formatGroupStarted = \ (nesting, name) -> interpret $ M.exampleGroupStarted formatter nesting name
, formatGroupDone = \(nesting, name) -> interpret (M.exampleGroupDone formatter nesting name)
, formatProgress = \path progress -> case progress of
    Started p -> do
      interpret $ M.exampleStarted formatter path
      when (p /= (0,0) && useColor) $
        interpret $ M.exampleProgress formatter path p
    Progress p -> when useColor $
      interpret $ M.exampleProgress formatter path p
, formatItem = \ path (Item loc _duration info result) ->
    case result of
      Success -> do
        increaseSuccessCount
        interpret $ M.exampleSucceeded formatter path info
      Pending reason -> do
        increasePendingCount
        interpret $ M.examplePending formatter path info reason
      Failure err -> do
        addFailMessage loc path err
        interpret $ M.exampleFailed formatter path info err
} where
    useColor = formatConfigUseColor config

interpret :: M.FormatM a -> FormatM a
interpret = interpretWith Environment {
  environmentGetSuccessCount = getSuccessCount
, environmentGetPendingCount = getPendingCount
, environmentGetFailMessages = getFailMessages
, environmentUsedSeed = usedSeed
, environmentGetCPUTime = getCPUTime
, environmentGetRealTime = getRealTime
, environmentWrite = writeTextBlock
, environmentRewrite = rewriteTextBlock
, environmentInsert  = insertTextBlock
, environmentUseDiff = gets (formatConfigUseDiff . stateConfig)
, environmentLiftIO = liftIO
}

-- | A lifted version of `Control.Monad.Trans.State.gets`
gets :: (FormatterState -> a) -> FormatM a
gets f = FormatM $ do
  f <$> (get >>= liftIO . readIORef)

-- | A lifted version of `Control.Monad.Trans.State.modify`
modify :: (FormatterState -> FormatterState) -> FormatM ()
modify f = FormatM $ do
  get >>= liftIO . (`modifyIORef'` f)

data FormatConfig = FormatConfig {
  formatConfigHandle :: Handle
, formatConfigUseColor :: Bool
, formatConfigUseDiff :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
} deriving (Eq, Show)

data FormatterState = FormatterState {
  stateSuccessCount    :: Int
, statePendingCount    :: Int
, stateBlocks          :: TextBlocks
, stateFailMessages    :: [FailureRecord]
, stateCpuStartTime    :: Maybe Integer
, stateStartTime       :: Seconds
, stateConfig          :: FormatConfig
}

getConfig :: (FormatConfig -> a) -> FormatM a
getConfig f = gets (f . stateConfig)

-- | The random seed that is used for QuickCheck.
usedSeed :: FormatM Integer
usedSeed = getConfig formatConfigUsedSeed

-- NOTE: We use an IORef here, so that the state persists when UserInterrupt is
-- thrown.
newtype FormatM a = FormatM (StateT (IORef FormatterState) (FreeT TextF IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

liftText :: FreeT TextF IO a -> FormatM a
liftText = FormatM . lift

runFormatM :: FormatConfig -> FormatM a -> FreeT TextF IO a
runFormatM config (FormatM action) = do
  time <- liftIO getMonotonicTime
  cpuTime <- if formatConfigPrintCpuTime config
             then Just <$> liftIO CPUTime.getCPUTime else return Nothing
  st <- liftIO$ newIORef (FormatterState 0 0 mempty [] cpuTime time config)
  evalStateT action st

-- | Increase the counter for successful examples
increaseSuccessCount :: FormatM ()
increaseSuccessCount = modify $ \s -> s {stateSuccessCount = succ $ stateSuccessCount s}

-- | Increase the counter for pending examples
increasePendingCount :: FormatM ()
increasePendingCount = modify $ \s -> s {statePendingCount = succ $ statePendingCount s}

-- | Get the number of successful examples encountered so far.
getSuccessCount :: FormatM Int
getSuccessCount = gets stateSuccessCount

-- | Get the number of pending examples encountered so far.
getPendingCount :: FormatM Int
getPendingCount = gets statePendingCount

-- | Append to the list of accumulated failure messages.
addFailMessage :: Maybe Location -> Path -> FailureReason -> FormatM ()
addFailMessage loc p m = modify $ \s -> s {stateFailMessages = FailureRecord loc p m : stateFailMessages s}

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = reverse `fmap` gets stateFailMessages

writeTextBlock :: TextBlock () -> FormatM Int
writeTextBlock tb = do
  blocks <- gets stateBlocks
  let (blocks', ix) = appendTextBlock tb blocks
  modify $ \s -> s{stateBlocks = blocks'}
  void $ liftText $ renderTextBlock tb
  return ix

insertTextBlock :: Int -> TextBlock () -> FormatM Int
insertTextBlock l tb = do
  blocks <- gets stateBlocks
  let (blocks', ix) = insertTextBlockAt l tb blocks
  modify $ \s -> s { stateBlocks = blocks'}
  liftText $ reRenderTextBlocks blocks blocks'
  return ix

rewriteTextBlock
  :: Int -> (TextBlock () -> Maybe (TextBlock ())) -> FormatM ()
rewriteTextBlock l f = do
  blocks <- gets stateBlocks
  let blocks' = modifyTextBlock l f blocks
  modify $ \s -> s{stateBlocks = blocks'}
  liftText $ reRenderTextBlocks blocks blocks'

-- |
-- @finally_ actionA actionB@ runs @actionA@ and then @actionB@.  @actionB@ is
-- run even when a `UserInterrupt` occurs during @actionA@.
finally_ :: FormatM a -> FormatM () -> FormatM a
finally_ (FormatM actionA) (FormatM actionB) = FormatM . StateT $ \st -> do
  r <- try (runStateT actionA st)
  case r of
    Left e -> do
      when (e == UserInterrupt) $
        runStateT actionB st >> return ()
      throwM e
    Right (a, st_) -> do
      runStateT actionB st_ >>= return . replaceValue a
  where
    replaceValue a (_, st) = (a, st)

-- | Get the used CPU time since the test run has been started.
getCPUTime :: FormatM (Maybe Seconds)
getCPUTime = do
  t1  <- liftIO CPUTime.getCPUTime
  mt0 <- gets stateCpuStartTime
  return $ toSeconds <$> ((-) <$> pure t1 <*> mt0)
  where
    toSeconds x = Seconds (fromIntegral x / (10.0 ^ (12 :: Integer)))

-- | Get the passed real time since the test run has been started.
getRealTime :: FormatM Seconds
getRealTime = do
  t1 <- liftIO getMonotonicTime
  t0 <- gets stateStartTime
  return (t1 - t0)

-- -----------
-- Text blocks
-- -----------
runTextIO :: (MonadIO m, MonadMask m) => Handle -> FormatConfig -> FreeT TextF m b -> m b
runTextIO h config act = interpretText (renderEnv h config) act

renderEnv, renderText, renderHtml :: (MonadIO m, MonadMask m) => Handle -> FormatConfig -> TextEnvironment m
renderEnv h cfg
  | formatConfigHtmlOutput cfg = renderHtml h cfg
  | otherwise = renderText h cfg

renderHtml h _ = TextEnvironment{..}
  where
    envClearFromCursorToEnd = return ()
    envClearLine = return ()
    envMoveCursorUp _ = return ()
    envRenderTextSpan tb
      | Just cls <- styleToClass (tsStyle tb)
      = liftIO $ IO.hPutStrLn h $ "<span class=\"" ++ cls ++ "\">" ++ tsText tb ++ "</span>"
      | otherwise
      = liftIO $ IO.hPutStrLn h $ tsText tb
    styleToClass InfoStyle    = Just "hspec-info"
    styleToClass FailureStyle = Just "hspec-failure"
    styleToClass PendingStyle = Just "hspec-pending"
    styleToClass SuccessStyle = Just "hspec-success"
    styleToClass DiffExtraStyle   = Just "hspec-failure"
    styleToClass DiffMissingStyle = Just "hspec-success"
    styleToClass _ = Nothing

renderText h config = TextEnvironment{..}
  where
    envRenderTextSpan tb = liftIO $ do
      let sgr = styleToSGR config (tsStyle tb) (tsText tb)
          codes =
            setSGRCode' sgr ++
            tsText tb ++
            resetSGRCode sgr
      IO.hPutStr h codes

    setSGRCode' [] = []
    setSGRCode' other = setSGRCode other

    resetSGRCode [] = []
    resetSGRCode _  = setSGRCode [Reset]

    envMoveCursorUp n = liftIO $ do
      case n of
        0 -> return ()
        _ | n>0 -> IO.hPutStr h $ cursorUpLineCode n
        _       -> IO.hPutStr h $ cursorDownLineCode (-n)

    envClearFromCursorToEnd = liftIO $ hClearFromCursorToScreenEnd h

    envClearLine = liftIO $ hClearLine h

    styleToSGR :: FormatConfig -> Style -> String -> [SGR]
    styleToSGR cfg InfoStyle        _ | formatConfigUseColor cfg = [SetColor Foreground Dull Cyan]
    styleToSGR cfg FailureStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Red]
    styleToSGR cfg SuccessStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Green]
    styleToSGR cfg PendingStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Yellow]
    styleToSGR cfg DiffMissingStyle t | formatConfigUseColor cfg && formatConfigUseDiff cfg = diffColorize Green t
    styleToSGR cfg DiffExtraStyle   t | formatConfigUseColor cfg && formatConfigUseDiff cfg = diffColorize Red t
    styleToSGR _   _ _ = []

    diffColorize :: Color -> String-> [SGR]
    diffColorize color s = [SetColor layer Dull color]
      where
        layer
          | all isSpace s = Background
          | otherwise = Foreground

