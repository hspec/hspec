-- | This module contains formaatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
module Test.Hspec.Formatters (
  restoreFormat,
  silent, specdoc, progress, failed_examples
) where

import Test.Hspec.Core
import System.IO
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)
import System.Console.ANSI

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
    when useColor (restoreFormat h),

  examplePassed = \ h spec _ -> do
    when useColor (passColor h)
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec
    when useColor (restoreFormat h),

  exampleFailed = \ h spec errors -> do
    when useColor (failColor h)
    hPutStrLn h $ indentationFor spec ++ " x " ++ requirement spec ++ " FAILED [" ++ (show $ (length errors) + 1) ++ "]"
    when useColor (restoreFormat h),

  examplePending = \ h spec _ -> do
    when useColor (pendingColor h)
    let (Pending s) = result spec
    hPutStrLn h $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s
    when useColor (restoreFormat h),

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (restoreFormat h)
  }


progress :: Bool -> Formatter
progress useColor = (silent useColor) {
  formatterName = "progress",

  examplePassed = \ h _ _ -> do
    when useColor (passColor h)
    hPutStr h "."
    when useColor (restoreFormat h),

  exampleFailed = \ h _ _ -> do
    when useColor (failColor h)
    hPutStr h "F"
    when useColor (restoreFormat h),

  examplePending = \ h _ _ -> do
    when useColor (pendingColor h)
    hPutStr h $ "."
    when useColor (restoreFormat h),

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (restoreFormat h)
  }


failed_examples :: Bool -> Formatter
failed_examples useColor = (silent useColor) {
  formatterName = "failed_examples",

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ("" : intersperse "" errors)
    when (not $ null errors) (hPutStrLn h "")
    when useColor (restoreFormat h),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (restoreFormat h)
  }


failColor :: Handle -> IO()
failColor h = hSetSGR h [ SetColor Foreground Dull Red ]

passColor :: Handle -> IO()
passColor h = hSetSGR h [ SetColor Foreground Dull Green ]

pendingColor :: Handle -> IO()
pendingColor h = hSetSGR h [ SetColor Foreground Dull Yellow ]

normalColor :: Handle -> IO()
normalColor h = hSetSGR h [ Reset ]

restoreFormat :: Handle -> IO()
restoreFormat h = hSetSGR h [ Reset ]
