-- | This module contains formatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
module Test.Hspec.Formatters (

-- * Formatters
  silent
, specdoc
, progress
, failed_examples

-- * Implementing a custom Formatter
-- |
-- A formatter is a set of actions.  Each action is evaluated when a certain
-- situation is encountered during a test run.
--
-- Actions live in the `FormatM` monad.  It provides access to the runner state
-- and primitives for appending to the generated report.
, Formatter (..)
, FormatM

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount
, getFailMessages

-- ** Appending to the gerenated report
, write
, writeLine

-- ** Dealing with colors
, withNormalColor
, withSuccessColor
, withPendingColor
, withFailColor
) where

import Test.Hspec.Core (quantify)
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Formatters.Internal (
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
  )


silent :: Formatter
silent = Formatter {
  formatterName       = "silent"
, exampleGroupStarted = \_ _ -> return ()
, exampleSucceeded    = \_ _ -> return ()
, exampleFailed       = \_ _ _ -> return ()
, examplePending      = \_ _ _  -> return ()
, failedFormatter     = return ()
, footerFormatter     = \_ -> return ()
}


specdoc :: Formatter
specdoc = silent {
  formatterName = "specdoc"

, exampleGroupStarted = \nesting name -> withNormalColor $ do
    writeLine ("\n" ++ indentationForGroup nesting ++ name)

, exampleSucceeded = \nesting requirement -> withSuccessColor $ do
    writeLine $ indentationForExample nesting ++ " - " ++ requirement

, exampleFailed = \nesting requirement _ -> withFailColor $ do
    failed <- getFailCount
    writeLine $ indentationForExample nesting ++ " - " ++ requirement ++ " FAILED [" ++ show failed ++ "]"

, examplePending = \nesting requirement reason -> withPendingColor $ do
    writeLine $ indentationForExample nesting ++ " - " ++ requirement ++ "\n     # " ++ reason

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationForExample nesting = replicate (pred nesting * 2) ' '
    indentationForGroup nesting = replicate (nesting * 2) ' '


progress :: Formatter
progress = silent {
  formatterName    = "progress"
, exampleSucceeded = \_ _ -> withSuccessColor $ write "."
, exampleFailed    = \_ _ _ -> withFailColor    $ write "F"
, examplePending   = \_ _ _ -> withPendingColor $ write "."
, failedFormatter  = defaultFailedFormatter
, footerFormatter  = defaultFooter
}


failed_examples :: Formatter
failed_examples   = silent {
  formatterName   = "failed_examples"
, failedFormatter = defaultFailedFormatter
, footerFormatter = defaultFooter
}


defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = withFailColor $ do
  failures <- getFailMessages
  mapM_ writeLine ("" : intersperse "" failures)
  when (not $ null failures) (writeLine "")

defaultFooter :: Double -> FormatM ()
defaultFooter time = do
  fails <- getFailCount
  total <- getTotalCount
  (if fails == 0 then withSuccessColor else withFailColor) $ do
    writeLine $ printf "Finished in %1.4f seconds" time
    writeLine ""
    write $ quantify total "example" ++ ", "
    writeLine $ quantify fails "failure"
