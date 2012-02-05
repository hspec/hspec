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

  exampleGroupStarted = \spec -> withNormalColor $ do
    writeLine ("\n" ++ indentationFor spec ++ name spec)
    ,

  examplePassed = \spec -> withPassColor $ do
    writeLine $ indentationFor spec ++ " - " ++ requirement spec
    ,

  exampleFailed = \spec -> withFailColor $ do
    errors <- getFailCount
    writeLine $ indentationFor spec ++ " - " ++ requirement spec ++ " FAILED [" ++ show errors ++ "]"
    ,

  examplePending = \spec -> withPendingColor $ do
    let (Pending s) = result spec
    writeLine $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s
    ,

  errorsFormatter = defaultErrorsFormatter,

  footerFormatter = defaultFooter
  }


progress :: Formatter
progress = silent {
  formatterName = "progress",

  examplePassed = \_ -> withPassColor $ do
    write "."
    ,

  exampleFailed = \_ -> withFailColor $ do
    write "F"
    ,

  examplePending = \_ -> withPendingColor $ do
    write $ "."
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
defaultErrorsFormatter = withFailColor $ do
  errors <- getFailMessages
  mapM_ writeLine ("" : intersperse "" errors)
  when (not $ null errors) (writeLine "")

defaultFooter :: Double -> FormatM ()
defaultFooter time = do
  fails <- getFailCount
  total <- getTotalCount
  (if fails == 0 then withPassColor else withFailColor) $ do
    writeLine $ printf "Finished in %1.4f seconds" time
    writeLine ""
    write $ quantify total "example" ++ ", "
    writeLine $ quantify fails "failure"
