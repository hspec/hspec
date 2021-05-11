{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Formatters.Internal (
  FormatM
, runFormatM
, interpret
, increaseSuccessCount
, increasePendingCount
, addFailMessage
, formatterToFormat
#ifdef TEST
, overwriteWith
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified System.IO as IO
import           System.IO (Handle, stdout)
import           Control.Exception (bracket_)
import           System.Console.ANSI
import           Control.Monad.Trans.State hiding (state, gets, modify)
import           Control.Monad.IO.Class
import           Data.Char (isSpace)
import qualified System.CPUTime as CPUTime

import qualified Test.Hspec.Core.Formatters.Monad as M
import           Test.Hspec.Core.Formatters.Monad (Environment(..), interpretWith, FailureRecord(..))
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Clock

formatterToFormat :: M.Formatter -> FormatConfig -> IO Format
formatterToFormat M.Formatter{..} config = monadic (runFormatM config) $ \ event -> case event of
  Started -> interpret formatterStarted
  GroupStarted path -> interpret $ formatterGroupStarted path
  GroupDone path -> interpret $ formatterGroupDone path
  Progress path progress -> interpret $ formatterProgress path progress
  ItemStarted path -> interpret $ formatterItemStarted path
  ItemDone path item -> do
    clearTransientOutput
    case itemResult item of
      Success {} -> increaseSuccessCount
      Pending {} -> increasePendingCount
      Failure loc err -> addFailMessage (loc <|> itemLocation item) path err
    interpret $ formatterItemDone path item
  Done _ -> interpret formatterDone

interpret :: M.FormatM a -> FormatM a
interpret = interpretWith Environment {
  environmentGetSuccessCount = getSuccessCount
, environmentGetPendingCount = getPendingCount
, environmentGetFailMessages = getFailMessages
, environmentGetFinalCount = getItemCount
, environmentUsedSeed = usedSeed
, environmentPrintTimes = gets (formatConfigPrintTimes . stateConfig)
, environmentGetCPUTime = getCPUTime
, environmentGetRealTime = getRealTime
, environmentWrite = write
, environmentWriteTransient = writeTransient
, environmentWithFailColor = withFailColor
, environmentWithSuccessColor = withSuccessColor
, environmentWithPendingColor = withPendingColor
, environmentWithInfoColor = withInfoColor
, environmentUseDiff = gets (formatConfigUseDiff . stateConfig)
, environmentExtraChunk = extraChunk
, environmentMissingChunk = missingChunk
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

data FormatterState = FormatterState {
  stateSuccessCount    :: !Int
, statePendingCount    :: !Int
, stateFailMessages    :: [FailureRecord]
, stateCpuStartTime    :: Maybe Integer
, stateStartTime       :: Seconds
, stateTransientOutput :: String
, stateConfig :: FormatConfig
}

getConfig :: (FormatConfig -> a) -> FormatM a
getConfig f = gets (f . stateConfig)

getHandle :: FormatM Handle
getHandle = return stdout

-- | The random seed that is used for QuickCheck.
usedSeed :: FormatM Integer
usedSeed = getConfig formatConfigUsedSeed

-- NOTE: We use an IORef here, so that the state persists when UserInterrupt is
-- thrown.
newtype FormatM a = FormatM (StateT (IORef FormatterState) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runFormatM :: FormatConfig -> FormatM a -> IO a
runFormatM config (FormatM action) = do
  time <- getMonotonicTime
  cpuTime <- if (formatConfigPrintCpuTime config) then Just <$> CPUTime.getCPUTime else pure Nothing
  st <- newIORef (FormatterState 0 0 [] cpuTime time "" config)
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

-- | Get the number of spec items that will have been encountered when this run
-- completes (if it is not terminated early).
getItemCount :: FormatM Int
getItemCount = getConfig formatConfigItemCount

overwriteWith :: String -> String -> String
overwriteWith old new
  | n == 0 = new
  | otherwise = '\r' : new ++ replicate (n - length new) ' '
  where
    n = length old

writeTransient :: String -> FormatM ()
writeTransient new = do
  useColor <- getConfig formatConfigUseColor
  when (useColor) $ do
    old <- gets stateTransientOutput
    write $ old `overwriteWith` new
    modify $ \ state -> state {stateTransientOutput = new}
    h <- getHandle
    liftIO $ IO.hFlush h

clearTransientOutput :: FormatM ()
clearTransientOutput = do
  n <- length <$> gets stateTransientOutput
  unless (n == 0) $ do
    write ("\r" ++ replicate n ' ' ++ "\r")
    modify $ \ state -> state {stateTransientOutput = ""}

-- | Append some output to the report.
write :: String -> FormatM ()
write s = do
  h <- getHandle
  liftIO $ IO.hPutStr h s

-- | Set output color to red, run given action, and finally restore the default
-- color.
withFailColor :: FormatM a -> FormatM a
withFailColor = withColor (SetColor Foreground Dull Red) "hspec-failure"

-- | Set output color to green, run given action, and finally restore the
-- default color.
withSuccessColor :: FormatM a -> FormatM a
withSuccessColor = withColor (SetColor Foreground Dull Green) "hspec-success"

-- | Set output color to yellow, run given action, and finally restore the
-- default color.
withPendingColor :: FormatM a -> FormatM a
withPendingColor = withColor (SetColor Foreground Dull Yellow) "hspec-pending"

-- | Set output color to cyan, run given action, and finally restore the
-- default color.
withInfoColor :: FormatM a -> FormatM a
withInfoColor = withColor (SetColor Foreground Dull Cyan) "hspec-info"

-- | Set a color, run an action, and finally reset colors.
withColor :: SGR -> String -> FormatM a -> FormatM a
withColor color cls action = do
  produceHTML <- getConfig formatConfigHtmlOutput
  (if produceHTML then htmlSpan cls else withColor_ color) action

htmlSpan :: String -> FormatM a -> FormatM a
htmlSpan cls action = write ("<span class=\"" ++ cls ++ "\">") *> action <* write "</span>"

withColor_ :: SGR -> FormatM a -> FormatM a
withColor_ color (FormatM action) = do
  useColor <- getConfig formatConfigUseColor
  h <- getHandle

  FormatM . StateT $ \st -> do
    bracket_

      -- set color
      (when useColor $ hSetSGR h [color])

      -- reset colors
      (when useColor $ hSetSGR h [Reset])

      -- run action
      (runStateT action st)

-- | Output given chunk in red.
extraChunk :: String -> FormatM ()
extraChunk s = do
  useDiff <- getConfig formatConfigUseDiff
  case useDiff of
    True -> extra s
    False -> write s
  where
    extra :: String -> FormatM ()
    extra = diffColorize Red "hspec-failure"

-- | Output given chunk in green.
missingChunk :: String -> FormatM ()
missingChunk s = do
  useDiff <- getConfig formatConfigUseDiff
  case useDiff of
    True -> missing s
    False -> write s
  where
    missing :: String-> FormatM ()
    missing = diffColorize Green "hspec-success"

diffColorize :: Color -> String -> String-> FormatM ()
diffColorize color cls s = withColor (SetColor layer Dull color) cls $ do
  write s
  where
    layer
      | all isSpace s = Background
      | otherwise = Foreground

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
