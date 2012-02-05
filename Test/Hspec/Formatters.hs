-- | This module contains formatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
module Test.Hspec.Formatters (
  restoreFormat,
  silent, specdoc, progress, failed_examples
  , Formatter (..)
  , FormatM
  , runFormatM
  , increaseSuccessCount
  , increasePendingCount
  , increaseFailCount
) where

import Test.Hspec.Core
import qualified System.IO as IO
import System.IO (Handle)
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)
import System.Console.ANSI
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data FormatterState = FormatterState {
  stateHandle   :: Handle
, stateUseColor :: Bool
, successCount  :: Int
, pendingCount  :: Int
, failCount     :: Int
}

totalCount :: FormatterState -> Int
totalCount s = successCount s + pendingCount s + failCount s

type FormatM = StateT FormatterState IO

runFormatM :: Bool -> Handle -> FormatM a -> IO a
runFormatM useColor handle action = evalStateT action (FormatterState handle useColor 0 0 0)

increaseSuccessCount :: FormatM ()
increaseSuccessCount = modify $ \s -> s {successCount = succ $ successCount s}

increasePendingCount :: FormatM ()
increasePendingCount = modify $ \s -> s {pendingCount = succ $ pendingCount s}

increaseFailCount :: FormatM ()
increaseFailCount = modify $ \s -> s {failCount = succ $ failCount s}

data Formatter = Formatter {
  formatterName       :: String
, exampleGroupStarted :: Spec -> FormatM ()
, examplePassed       :: Spec -> FormatM ()
, exampleFailed       :: Spec -> FormatM ()
, examplePending      :: Spec -> FormatM ()
, errorsFormatter     :: [String] -> FormatM ()
, footerFormatter     :: Double -> FormatM ()
}

hPutStrLn :: Handle -> String -> FormatM ()
hPutStrLn h = liftIO . IO.hPutStrLn h

hPutStr :: Handle -> String -> FormatM ()
hPutStr h = liftIO . IO.hPutStr h


silent :: Formatter
silent = Formatter {
  formatterName = "silent",
  exampleGroupStarted = \_ -> return (),
  examplePassed = \_ -> return (),
  exampleFailed = \_ -> return (),
  examplePending = \_ -> return (),
  errorsFormatter = \_ -> return (),
  footerFormatter = \_ -> return ()
  }

indentationFor :: Spec -> String
indentationFor spec = replicate (depth spec * 2) ' '

specdoc :: Formatter
specdoc = silent {
  formatterName = "specdoc",

  exampleGroupStarted = \spec -> withNormalColor $ \h -> do
    hPutStrLn h ("\n" ++ indentationFor spec ++ name spec)
    ,

  examplePassed = \spec -> withPassColor $ \h -> do
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec
    ,

  exampleFailed = \spec -> withFailColor $ \h -> do
    errors <- gets failCount
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ " FAILED [" ++ show errors ++ "]"
    ,

  examplePending = \spec -> withPendingColor $ \h -> do
    let (Pending s) = result spec
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s
    ,

  errorsFormatter = defaultErrorsFormatter,

  footerFormatter = defaultFooter
  }


progress :: Formatter
progress = silent {
  formatterName = "progress",

  examplePassed = \_ -> withPassColor $ \h -> do
    hPutStr h "."
    ,

  exampleFailed = \_ -> withFailColor $ \h -> do
    hPutStr h "F"
    ,

  examplePending = \_ -> withPendingColor $ \h -> do
    hPutStr h $ "."
    ,

  errorsFormatter = defaultErrorsFormatter,

  footerFormatter = defaultFooter
  }


failed_examples :: Formatter
failed_examples = silent {
  formatterName = "failed_examples",

  errorsFormatter = defaultErrorsFormatter,

  footerFormatter = defaultFooter
  }

defaultErrorsFormatter :: [String] -> FormatM ()
defaultErrorsFormatter errors = withFailColor $ \h -> do
  mapM_ (hPutStrLn h) ("" : intersperse "" errors)
  when (not $ null errors) (hPutStrLn h "")

defaultFooter :: Double -> FormatM ()
defaultFooter time = do
  fails <- gets failCount
  total <- gets totalCount
  (if fails == 0 then withPassColor else withFailColor) $ \h -> do
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify total "example" ++ ", "
    hPutStrLn h $ quantify fails "failure"


withFailColor :: (Handle -> FormatM a) -> FormatM a
withFailColor = withColor (SetColor Foreground Dull Red)

withPassColor :: (Handle -> FormatM a) -> FormatM a
withPassColor = withColor (SetColor Foreground Dull Green)

withPendingColor :: (Handle -> FormatM a) -> FormatM a
withPendingColor = withColor (SetColor Foreground Dull Yellow)

withNormalColor :: (Handle -> FormatM a) -> FormatM a
withNormalColor = withColor Reset

-- | Set a color, run an action, and finally reset colors.
withColor :: SGR -> (Handle -> FormatM a) -> FormatM a
withColor color action = do
  useColor <- gets stateUseColor
  h <- gets stateHandle
  when useColor (liftIO $ hSetSGR h [color])
  r <- action h

  -- FIXME:  When action throws an exception, restoreFormat is never called.
  -- We can remedy this by using finally in combination with `monad-control`,
  -- `MonadCatchIO-transformers`, or something.
  when useColor (liftIO $ restoreFormat h)
  return r

restoreFormat :: Handle -> IO ()
restoreFormat h = hSetSGR h [ Reset ]
