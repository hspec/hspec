{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Formatters.Internal (

-- * Public API
  Formatter (..)
, FormatM

, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount
, getFailMessages

, write
, writeLine

, withNormalColor
, withSuccessColor
, withPendingColor
, withFailColor

-- * Functions for internal use
, runFormatM
, liftIO
, increaseSuccessCount
, increasePendingCount
, increaseFailCount
, addFailMessage
, restoreFormat
) where

import qualified System.IO as IO
import System.IO (Handle)
import Control.Monad (when)
import System.Console.ANSI
import Control.Monad.Trans.State hiding (gets, modify)
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.IO.Class as IOClass

-- | A lifted version of `State.gets`
gets :: (FormatterState -> a) -> FormatM a
gets f = FormatM (State.gets f)

-- | A lifted version of `State.modify`
modify :: (FormatterState -> FormatterState) -> FormatM ()
modify f = FormatM (State.modify f)

-- | A lifted version of `IOClass.liftIO`
--
-- This is meant for internal use only, and not part of the public API.  This
-- is also the reason why we do not make FormatM an instance MonadIO, so we
-- have narrow control over the visibilty of this function.
liftIO :: IO a -> FormatM a
liftIO action = FormatM (IOClass.liftIO action)

data FormatterState = FormatterState {
  stateHandle   :: Handle
, stateUseColor :: Bool
, successCount  :: Int
, pendingCount  :: Int
, failCount     :: Int
, failMessages  :: [String]
}

-- | The total number of examples encountered so far.
totalCount :: FormatterState -> Int
totalCount s = successCount s + pendingCount s + failCount s

newtype FormatM a = FormatM (StateT FormatterState IO a)
  deriving (Functor, Monad)

runFormatM :: Bool -> Handle -> FormatM a -> IO a
runFormatM useColor handle (FormatM action) = evalStateT action (FormatterState handle useColor 0 0 0 [])

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
addFailMessage :: String -> FormatM ()
addFailMessage err = modify $ \s -> s {failMessages = err : failMessages s}

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [String]
getFailMessages = reverse `fmap` gets failMessages

data Formatter = Formatter {
  formatterName       :: String

-- | evaluated before each test group
, exampleGroupStarted :: Int -> String -> FormatM ()
-- | evaluated after each successful example
, exampleSucceeded    :: Int -> String -> FormatM ()
-- | evaluated after each failed example
, exampleFailed       :: Int -> String -> String -> FormatM ()
-- | evaluated after each pending example
, examplePending      :: Int -> String -> String -> FormatM ()
-- | evaluated after a test run
, failedFormatter     :: FormatM ()
-- | evaluated after `failuresFormatter`
, footerFormatter     :: Double -> FormatM ()
}

-- | Append some output to the report.
write :: String -> FormatM ()
write s = do
  h <- gets stateHandle
  liftIO $ IO.hPutStr h s

-- | The same as `write`, but adds a newline character.
writeLine :: String -> FormatM ()
writeLine s = do
  h <- gets stateHandle
  liftIO $ IO.hPutStrLn h s

-- | Set output color to red, run given action, and finally restore the default
-- color.
withFailColor :: FormatM a -> FormatM a
withFailColor = withColor (SetColor Foreground Dull Red)

-- | Set output to color green, run given action, and finally restore the
-- default color.
withSuccessColor :: FormatM a -> FormatM a
withSuccessColor = withColor (SetColor Foreground Dull Green)

-- | Set output color to yellow, run given action, and finally restore the
-- default color.
withPendingColor :: FormatM a -> FormatM a
withPendingColor = withColor (SetColor Foreground Dull Yellow)

withNormalColor :: FormatM a -> FormatM a
withNormalColor = withColor Reset

-- | Set a color, run an action, and finally reset colors.
withColor :: SGR -> FormatM a -> FormatM a
withColor color action = do
  useColor <- gets stateUseColor
  h <- gets stateHandle
  when useColor (liftIO $ hSetSGR h [color])
  r <- action

  -- FIXME:  When action throws an exception, restoreFormat is never called.
  -- We can remedy this by using finally in combination with `monad-control`,
  -- `MonadCatchIO-transformers`, or something.
  when useColor (liftIO $ restoreFormat h)
  return r

restoreFormat :: Handle -> IO ()
restoreFormat h = hSetSGR h [ Reset ]
