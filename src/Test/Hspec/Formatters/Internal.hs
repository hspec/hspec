{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Formatters.Internal (

-- * Public API
  Formatter (..)
, FormatM

, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, getCPUTime
, getRealTime

, write
, writeLine
, newParagraph

, withSuccessColor
, withPendingColor
, withFailColor

-- * Functions for internal use
, runFormatM
, increaseSuccessCount
, increasePendingCount
, increaseFailCount
, addFailMessage
, finally_
) where

import qualified System.IO as IO
import           System.IO (Handle)
import           Control.Monad (when, unless)
import           Control.Applicative
import           Control.Exception (SomeException, AsyncException(..), bracket_, try, throwIO)
import           System.Console.ANSI
import           Control.Monad.Trans.State hiding (gets, modify)
import           Control.Monad.IO.Class
import qualified System.CPUTime as CPUTime
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import           Test.Hspec.Util (Path)
import           Test.Hspec.Compat
import           Test.Hspec.Core.Type (Progress)

-- | A lifted version of `Control.Monad.Trans.State.gets`
gets :: (FormatterState -> a) -> FormatM a
gets f = FormatM $ do
  f <$> (get >>= liftIO . readIORef)

-- | A lifted version of `Control.Monad.Trans.State.modify`
modify :: (FormatterState -> FormatterState) -> FormatM ()
modify f = FormatM $ do
  get >>= liftIO . (`modifyIORef'` f)

data FormatterState = FormatterState {
  stateHandle     :: Handle
, stateUseColor   :: Bool
, produceHTML     :: Bool
, lastIsEmptyLine :: Bool    -- True, if last line was empty
, successCount    :: Int
, pendingCount    :: Int
, failCount       :: Int
, failMessages    :: [FailureRecord]
, stateUsedSeed   :: Integer
, cpuStartTime    :: Maybe Integer
, startTime       :: POSIXTime
}

-- | The random seed that is used for QuickCheck.
usedSeed :: FormatM Integer
usedSeed = gets stateUsedSeed

-- | The total number of examples encountered so far.
totalCount :: FormatterState -> Int
totalCount = sum . sequence [successCount, pendingCount, failCount]

-- NOTE: We use an IORef here, so that the state persists when UserInterrupt is
-- thrown.
newtype FormatM a = FormatM (StateT (IORef FormatterState) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runFormatM :: Bool -> Bool -> Bool -> Integer -> Handle -> FormatM a -> IO a
runFormatM useColor produceHTML_ printCpuTime seed handle (FormatM action) = do
  time <- getPOSIXTime
  cpuTime <- if printCpuTime then Just <$> CPUTime.getCPUTime else pure Nothing
  st <- newIORef (FormatterState handle useColor produceHTML_ False 0 0 0 [] seed cpuTime time)
  evalStateT action st

-- | Increase the counter for successful examples
increaseSuccessCount :: FormatM ()
increaseSuccessCount = modify $ \s -> s {successCount = succ $ successCount s}

-- | Increase the counter for pending examples
increasePendingCount :: FormatM ()
increasePendingCount = modify $ \s -> s {pendingCount = succ $ pendingCount s}

-- | Increase the counter for failed examples
increaseFailCount :: FormatM ()
increaseFailCount = modify $ \s -> s {failCount = succ $ failCount s}

-- | Get the number of successful examples encountered so far.
getSuccessCount :: FormatM Int
getSuccessCount = gets successCount

-- | Get the number of pending examples encountered so far.
getPendingCount :: FormatM Int
getPendingCount = gets pendingCount

-- | Get the number of failed examples encountered so far.
getFailCount :: FormatM Int
getFailCount = gets failCount

-- | Get the total number of examples encountered so far.
getTotalCount :: FormatM Int
getTotalCount = gets totalCount

-- | Append to the list of accumulated failure messages.
addFailMessage :: Path -> Either SomeException String -> FormatM ()
addFailMessage p m = modify $ \s -> s {failMessages = FailureRecord p m : failMessages s}

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = reverse `fmap` gets failMessages

data FailureRecord = FailureRecord {
  failureRecordPath     :: Path
, failureRecordMessage  :: Either SomeException String
}

data Formatter = Formatter {

  headerFormatter :: FormatM ()

-- | evaluated before each test group
--
-- The given number indicates the position within the parent group.
, exampleGroupStarted :: Int -> [String] -> String -> FormatM ()

, exampleGroupDone    :: FormatM ()

-- | used to notify the progress of the currently evaluated example
--
-- NOTE: This is only called when interactive/color mode.
, exampleProgress     :: Handle -> Path -> Progress -> IO ()

-- | evaluated after each successful example
, exampleSucceeded    :: Path -> FormatM ()

-- | evaluated after each failed example
, exampleFailed       :: Path -> Either SomeException String -> FormatM ()

-- | evaluated after each pending example
, examplePending      :: Path -> Maybe String -> FormatM ()

-- | evaluated after a test run
, failedFormatter     :: FormatM ()

-- | evaluated after `failuresFormatter`
, footerFormatter     :: FormatM ()
}


-- | Append an empty line to the report.
--
-- Calling this multiple times has the same effect as calling it once.
newParagraph :: FormatM ()
newParagraph = do
  f <- gets lastIsEmptyLine
  unless f $ do
    writeLine ""
    setLastIsEmptyLine True

setLastIsEmptyLine :: Bool -> FormatM ()
setLastIsEmptyLine f = modify $ \s -> s {lastIsEmptyLine = f}

-- | Append some output to the report.
write :: String -> FormatM ()
write s = do
  h <- gets stateHandle
  liftIO $ IO.hPutStr h s
  setLastIsEmptyLine False

-- | The same as `write`, but adds a newline character.
writeLine :: String -> FormatM ()
writeLine s = write s >> write "\n"

-- | Set output color to red, run given action, and finally restore the default
-- color.
withFailColor :: FormatM a -> FormatM a
withFailColor = withColor (SetColor Foreground Dull Red) "hspec-failure"

-- | Set output to color green, run given action, and finally restore the
-- default color.
withSuccessColor :: FormatM a -> FormatM a
withSuccessColor = withColor (SetColor Foreground Dull Green) "hspec-success"

-- | Set output color to yellow, run given action, and finally restore the
-- default color.
withPendingColor :: FormatM a -> FormatM a
withPendingColor = withColor (SetColor Foreground Dull Yellow) "hspec-pending"

-- | Set a color, run an action, and finally reset colors.
withColor :: SGR -> String -> FormatM a -> FormatM a
withColor color cls action = do
  r <- gets produceHTML
  (if r then htmlSpan cls else withColor_ color) action

htmlSpan :: String -> FormatM a -> FormatM a
htmlSpan cls action = write ("<span class=\"" ++ cls ++ "\">") *> action <* write "</span>"

withColor_ :: SGR -> FormatM a -> FormatM a
withColor_ color (FormatM action) = do
  useColor <- gets stateUseColor
  h        <- gets stateHandle

  FormatM . StateT $ \st -> do
    bracket_

      -- set color
      (when useColor $ hSetSGR h [color])

      -- reset colors
      (when useColor $ hSetSGR h [Reset])

      -- run action
      (runStateT action st)

-- |
-- @finally_ actionA actionB@ runs @actionA@ and then @actionB@.  @actionB@ is
-- run even when a `UserInterrupt` occurs during @actionA@.
finally_ :: FormatM () -> FormatM () -> FormatM ()
finally_ (FormatM actionA) (FormatM actionB) = FormatM . StateT $ \st -> do
  r <- try (execStateT actionA st)
  case r of
    Left e -> do
      when (e == UserInterrupt) $
        runStateT actionB st >> return ()
      throwIO e
    Right st_ -> do
      runStateT actionB st_

-- | Get the used CPU time since the test run has been started.
getCPUTime :: FormatM (Maybe Double)
getCPUTime = do
  t1  <- liftIO CPUTime.getCPUTime
  mt0 <- gets cpuStartTime
  return $ toSeconds <$> ((-) <$> pure t1 <*> mt0)
  where
    toSeconds x = fromIntegral x / (10.0 ^ (12 :: Integer))

-- | Get the passed real time since the test run has been started.
getRealTime :: FormatM Double
getRealTime = do
  t1 <- liftIO getPOSIXTime
  t0 <- gets startTime
  return (realToFrac $ t1 - t0)
