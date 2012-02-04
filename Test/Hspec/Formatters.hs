-- | This module contains formatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
module Test.Hspec.Formatters (
  restoreFormat,
  silent, specdoc, progress, failed_examples
  , Formatter (..)
  , FormatM
  , runFormatM
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
}

type FormatM = StateT FormatterState IO

runFormatM :: Bool -> Handle -> FormatM a -> IO a
runFormatM useColor handle action = evalStateT action (FormatterState handle useColor)

data Formatter = Formatter {
  formatterName       :: String
, exampleGroupStarted :: Spec -> FormatM ()
, examplePassed       :: Spec -> [String] -> FormatM ()
, exampleFailed       :: Spec -> [String] -> FormatM ()
, examplePending      :: Spec -> [String] -> FormatM ()
, errorsFormatter     :: [String] -> FormatM ()
, footerFormatter     :: [Spec] -> Double -> FormatM ()
, usesFormatting      :: Bool
}

hPutStrLn :: Handle -> String -> FormatM ()
hPutStrLn h = liftIO . IO.hPutStrLn h

hPutStr :: Handle -> String -> FormatM ()
hPutStr h = liftIO . IO.hPutStr h


silent :: Bool -> Formatter
silent useColor = Formatter {
  formatterName = "silent",
  exampleGroupStarted = \_ -> return (),
  examplePassed = \_ _ -> return (),
  exampleFailed = \_ _ -> return (),
  examplePending = \_ _ -> return (),
  errorsFormatter = \_ -> return (),
  footerFormatter = \_ _ -> return (),
  usesFormatting = useColor
  }

indentationFor :: Spec -> String
indentationFor spec = replicate (depth spec * 2) ' '

specdoc :: Bool -> Formatter
specdoc useColor = (silent useColor) {
  formatterName = "specdoc",

  exampleGroupStarted = \spec -> withNormalColor $ \h -> do
    hPutStrLn h ("\n" ++ indentationFor spec ++ name spec)
    ,

  examplePassed = \spec _ -> withPassColor $ \h -> do
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec
    ,

  exampleFailed = \spec errors -> withFailColor $ \h -> do
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ " FAILED [" ++ (show $ (length errors) + 1) ++ "]"
    ,

  examplePending = \spec _ -> withPendingColor $ \h -> do
    let (Pending s) = result spec
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s
    ,

  errorsFormatter = \errors -> withFailColor $ \h -> do
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    ,

  footerFormatter = \specs time -> (if failedCount specs == 0 then withPassColor else withFailColor) $ \h -> do
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
  }


progress :: Bool -> Formatter
progress useColor = (silent useColor) {
  formatterName = "progress",

  examplePassed = \_ _ -> withPassColor $ \h -> do
    hPutStr h "."
    ,

  exampleFailed = \_ _ -> withFailColor $ \h -> do
    hPutStr h "F"
    ,

  examplePending = \_ _ -> withPendingColor $ \h -> do
    hPutStr h $ "."
    ,

  errorsFormatter = \errors -> withFailColor $ \h -> do
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    ,

  footerFormatter = \specs time -> (if failedCount specs == 0 then withPassColor else withFailColor) $ \h -> do
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
  }


failed_examples :: Bool -> Formatter
failed_examples useColor = (silent useColor) {
  formatterName = "failed_examples",

  errorsFormatter = \errors -> withFailColor $ \h -> do
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    ,

  footerFormatter = \specs time -> (if failedCount specs == 0 then withPassColor else withFailColor) $ \h -> do
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
  }


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
