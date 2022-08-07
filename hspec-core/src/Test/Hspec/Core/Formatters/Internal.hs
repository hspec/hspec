{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Formatters.Internal (
  Formatter(..)
, Item(..)
, Result(..)
, FailureReason(..)
, FormatM
, formatterToFormat

, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount
, getExpectedTotalCount

, FailureRecord(..)
, getFailMessages
, usedSeed

, printTimes
, getCPUTime
, getRealTime

, write
, writeLine
, writeTransient

, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, outputUnicode

, useDiff
, prettyPrint
, prettyPrintFunction
, extraChunk
, missingChunk

#ifdef TEST
, runFormatM
, splitLines
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified System.IO as IO
import           System.IO (Handle, stdout)
import           Control.Exception (bracket_, bracket)
import           System.Console.ANSI
import           Control.Monad.Trans.State hiding (state, gets, modify)
import           Control.Monad.IO.Class
import           Data.Char (isSpace)
import           Data.List (groupBy)
import qualified System.CPUTime as CPUTime

import           Test.Hspec.Core.Formatters.V1.Monad (FailureRecord(..))
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Clock

data Formatter = Formatter {
-- | evaluated before a test run
  formatterStarted :: FormatM ()

-- | evaluated before each spec group
, formatterGroupStarted :: Path -> FormatM ()

-- | evaluated after each spec group
, formatterGroupDone :: Path -> FormatM ()

-- | used to notify the progress of the currently evaluated example
, formatterProgress :: Path -> Progress -> FormatM ()

-- | evaluated before each spec item
, formatterItemStarted :: Path -> FormatM ()

-- | evaluated after each spec item
, formatterItemDone :: Path -> Item -> FormatM ()

-- | evaluated after a test run
, formatterDone :: FormatM ()
}

formatterToFormat :: Formatter -> FormatConfig -> IO Format
formatterToFormat Formatter{..} config = monadic (runFormatM config) $ \ event -> case event of
  Started -> formatterStarted
  GroupStarted path -> formatterGroupStarted path
  GroupDone path -> formatterGroupDone path
  Progress path progress -> formatterProgress path progress
  ItemStarted path -> formatterItemStarted path
  ItemDone path item -> do
    case itemResult item of
      Success {} -> increaseSuccessCount
      Pending {} -> increasePendingCount
      Failure loc err -> addFailure $ FailureRecord (loc <|> itemLocation item) path err
    formatterItemDone path item
  Done _ -> formatterDone
  where
    addFailure r = modify $ \ s -> s { stateFailMessages = r : stateFailMessages s }

-- | Get the number of failed examples encountered so far.
getFailCount :: FormatM Int
getFailCount = length <$> getFailMessages

-- | Return `True` if the user requested colorized diffs, `False` otherwise.
useDiff :: FormatM Bool
useDiff = getConfig formatConfigUseDiff

-- | Return `True` if the user requested pretty diffs, `False` otherwise.
prettyPrint :: FormatM Bool
prettyPrint = maybe False (const True) <$> getConfig formatConfigPrettyPrintFunction
{-# DEPRECATED prettyPrint "use `prettyPrintFunction` instead" #-}

-- | Return a function for pretty-printing if the user requested pretty diffs,
-- `Nothing` otherwise.
--
-- @since 2.10.0
prettyPrintFunction :: FormatM (Maybe PrettyPrintFunction)
prettyPrintFunction = getConfig formatConfigPrettyPrintFunction

-- | Return `True` if the user requested unicode output, `False` otherwise.
outputUnicode :: FormatM Bool
outputUnicode = getConfig formatConfigOutputUnicode

-- | The same as `write`, but adds a newline character.
writeLine :: String -> FormatM ()
writeLine s = write s >> write "\n"

-- | Return `True` if the user requested time reporting for individual spec
-- items, `False` otherwise.
printTimes :: FormatM Bool
printTimes = gets (formatConfigPrintTimes . stateConfig)

-- | Get the total number of examples encountered so far.
getTotalCount :: FormatM Int
getTotalCount = sum <$> sequence [getSuccessCount, getFailCount, getPendingCount]

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
, stateConfig          :: FormatConfig
, stateColor           :: Maybe SGR
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
runFormatM config (FormatM action) = withLineBuffering $ do
  time <- getMonotonicTime
  cpuTime <- if (formatConfigPrintCpuTime config) then Just <$> CPUTime.getCPUTime else pure Nothing

  let
    progress = formatConfigReportProgress config && not (formatConfigHtmlOutput config)
    state = FormatterState {
      stateSuccessCount = 0
    , statePendingCount = 0
    , stateFailMessages = []
    , stateCpuStartTime = cpuTime
    , stateStartTime = time
    , stateConfig = config { formatConfigReportProgress = progress }
    , stateColor = Nothing
    }
  newIORef state >>= evalStateT action

withLineBuffering :: IO a -> IO a
withLineBuffering action = bracket (IO.hGetBuffering stdout) (IO.hSetBuffering stdout) $ \ _ -> do
  IO.hSetBuffering stdout IO.LineBuffering >> action

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

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = reverse `fmap` gets stateFailMessages

-- | Get the number of spec items that will have been encountered when this run
-- completes (if it is not terminated early).
getExpectedTotalCount :: FormatM Int
getExpectedTotalCount = getConfig formatConfigExpectedTotalCount

writeTransient :: String -> FormatM ()
writeTransient new = do
  reportProgress <- getConfig formatConfigReportProgress
  when (reportProgress) $ do
    h <- getHandle
    write $ new
    liftIO $ IO.hFlush h
    write $ "\r" ++ replicate (length new) ' ' ++ "\r"

-- | Append some output to the report.
write :: String -> FormatM ()
write = mapM_ writeChunk . splitLines

splitLines :: String -> [String]
splitLines = groupBy (\ a b -> isNewline a == isNewline b)
  where
    isNewline = (== '\n')

writeChunk :: String -> FormatM ()
writeChunk str = do
  h <- getHandle
  mColor <- gets stateColor
  liftIO $ case mColor of
    Just color | not (all isSpace str) -> bracket_
      (hSetSGR h [color])
      (hSetSGR h [Reset])
      (IO.hPutStr h str)
    _ -> IO.hPutStr h str

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
withColor_ color action = do
  oldColor <- gets stateColor
  setColor (Just color) *> action <* setColor oldColor

setColor :: Maybe SGR -> FormatM ()
setColor color = do
  useColor <- getConfig formatConfigUseColor
  when useColor $ do
    modify (\ state -> state { stateColor = color })

-- | Output given chunk in red.
extraChunk :: String -> FormatM ()
extraChunk s = do
  diff <- getConfig formatConfigUseDiff
  case diff of
    True -> extra s
    False -> write s
  where
    extra :: String -> FormatM ()
    extra = diffColorize Red "hspec-failure"

-- | Output given chunk in green.
missingChunk :: String -> FormatM ()
missingChunk s = do
  diff <- getConfig formatConfigUseDiff
  case diff of
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
