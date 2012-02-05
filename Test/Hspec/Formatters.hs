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
  , getFailCount
  , addFailMessage
) where

import Test.Hspec.Core
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)
import Control.Monad.Trans.State

import Test.Hspec.Formatters.Internal

silent :: Formatter
silent = Formatter {
  formatterName = "silent",
  exampleGroupStarted = \_ -> return (),
  examplePassed = \_ -> return (),
  exampleFailed = \_ -> return (),
  examplePending = \_ -> return (),
  errorsFormatter = return (),
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

defaultErrorsFormatter :: FormatM ()
defaultErrorsFormatter = withFailColor $ \h -> do
  errors <- getFailMessages
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
