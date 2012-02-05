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
, withPassColor
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
  , withPassColor
  , withPendingColor
  , withFailColor
  )

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
