-- | This module contains formatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
module Test.Hspec.Formatters (
  restoreFormat,
  silent, specdoc, progress, failed_examples
  , Formatter (..)
  , FormatM
  , runFormatM
  , FormatterState (..)
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

data FormatterState = FormatterState

type FormatM = StateT FormatterState IO

runFormatM :: FormatterState -> FormatM a -> IO a
runFormatM = flip evalStateT

data Formatter = Formatter { formatterName   :: String,
                             exampleGroupStarted :: Handle -> Spec -> FormatM (),
                             examplePassed   :: Handle -> Spec -> [String] -> FormatM (),
                             exampleFailed   :: Handle -> Spec -> [String] -> FormatM (),
                             examplePending  :: Handle -> Spec -> [String] -> FormatM (),
                             errorsFormatter :: Handle -> [String] -> FormatM (),
                             footerFormatter :: Handle -> [Spec] -> Double -> FormatM (),
                             usesFormatting  :: Bool }

hPutStrLn :: Handle -> String -> FormatM ()
hPutStrLn h = liftIO . IO.hPutStrLn h

hPutStr :: Handle -> String -> FormatM ()
hPutStr h = liftIO . IO.hPutStr h


silent :: Bool -> Formatter
silent useColor = Formatter {
  formatterName = "silent",
  exampleGroupStarted = \ _ _ -> return (),
  examplePassed = \ _ _ _ -> return (),
  exampleFailed = \ _ _ _ -> return (),
  examplePending = \ _ _ _ -> return (),
  errorsFormatter = \ _ _ -> return (),
  footerFormatter = \ _ _ _ -> return (),
  usesFormatting = useColor
  }

indentationFor :: Spec -> String
indentationFor spec = replicate (depth spec * 2) ' '

specdoc :: Bool -> Formatter
specdoc useColor = (silent useColor) {
  formatterName = "specdoc",

  exampleGroupStarted = \ h spec -> do
    when useColor (normalColor h)
    hPutStrLn h ("\n" ++ indentationFor spec ++ name spec)
    when useColor (liftIO $ restoreFormat h),

  examplePassed = \ h spec _ -> do
    when useColor (passColor h)
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec
    when useColor (liftIO $ restoreFormat h),

  exampleFailed = \ h spec errors -> do
    when useColor (failColor h)
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ " FAILED [" ++ (show $ (length errors) + 1) ++ "]"
    when useColor (liftIO $ restoreFormat h),

  examplePending = \ h spec _ -> do
    when useColor (pendingColor h)
    let (Pending s) = result spec
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s
    when useColor (liftIO $ restoreFormat h),

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (liftIO $ restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (liftIO $ restoreFormat h)
  }


progress :: Bool -> Formatter
progress useColor = (silent useColor) {
  formatterName = "progress",

  examplePassed = \ h _ _ -> do
    when useColor (passColor h)
    hPutStr h "."
    when useColor (liftIO $ restoreFormat h),

  exampleFailed = \ h _ _ -> do
    when useColor (failColor h)
    hPutStr h "F"
    when useColor (liftIO $ restoreFormat h),

  examplePending = \ h _ _ -> do
    when useColor (pendingColor h)
    hPutStr h $ "."
    when useColor (liftIO $ restoreFormat h),

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (liftIO $ restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (liftIO $ restoreFormat h)
  }


failed_examples :: Bool -> Formatter
failed_examples useColor = (silent useColor) {
  formatterName = "failed_examples",

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (liftIO $ restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (liftIO $ restoreFormat h)
  }


failColor :: Handle -> FormatM ()
failColor h = liftIO $ hSetSGR h [ SetColor Foreground Dull Red ]

passColor :: Handle -> FormatM ()
passColor h = liftIO $ hSetSGR h [ SetColor Foreground Dull Green ]

pendingColor :: Handle -> FormatM ()
pendingColor h = liftIO $ liftIO $ hSetSGR h [ SetColor Foreground Dull Yellow ]

normalColor :: Handle -> FormatM ()
normalColor h = liftIO $ hSetSGR h [ Reset ]

restoreFormat :: Handle -> IO()
restoreFormat h = hSetSGR h [ Reset ]
