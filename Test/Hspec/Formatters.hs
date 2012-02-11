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

import Test.Hspec.Core
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- there own formatters.
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
, exampleGroupStarted = \_ -> return ()
, exampleSucceeded    = \_ -> return ()
, exampleFailed       = \_ -> return ()
, examplePending      = \_ -> return ()
, failedFormatter     = return ()
, footerFormatter     = \_ -> return ()
}


specdoc :: Formatter
specdoc = silent {
  formatterName = "specdoc"

, exampleGroupStarted = \spec -> withNormalColor $ do
    writeLine ("\n" ++ indentationFor spec ++ name spec)

, exampleSucceeded = \spec -> withSuccessColor $ do
    writeLine $ indentationFor spec ++ " - " ++ requirement spec

, exampleFailed = \spec -> withFailColor $ do
    failed <- getFailCount
    writeLine $ indentationFor spec ++ " - " ++ requirement spec ++ " FAILED [" ++ show failed ++ "]"

, examplePending = \spec -> withPendingColor $ do
    let (Pending s) = result spec
    writeLine $ indentationFor spec ++ " - " ++ requirement spec ++ "\n     # " ++ s

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationFor spec = replicate (depth spec * 2) ' '


progress :: Formatter
progress = silent {
  formatterName    = "progress"
, exampleSucceeded = \_ -> withSuccessColor $ write "."
, exampleFailed    = \_ -> withFailColor    $ write "F"
, examplePending   = \_ -> withPendingColor $ write "."
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
