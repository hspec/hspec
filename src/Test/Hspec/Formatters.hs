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

, FailureRecord (..)
, getFailMessages

, getCPUTime
, getRealTime

-- ** Appending to the gerenated report
, write
, writeLine

-- ** Dealing with colors
, withSuccessColor
, withPendingColor
, withFailColor
) where

import           Data.Maybe
import           Test.Hspec.Util
import           Data.List (intersperse)
import           Text.Printf
import           Control.Monad (unless)
import           Control.Applicative
import qualified Control.Exception as E
import           Data.Typeable (typeOf)

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

  , FailureRecord (..)
  , getFailMessages

  , getCPUTime
  , getRealTime

  , write
  , writeLine

  , withSuccessColor
  , withPendingColor
  , withFailColor
  )


silent :: Formatter
silent = Formatter {
  exampleGroupStarted = \_ _ -> return ()
, exampleSucceeded    = \_ _ -> return ()
, exampleFailed       = \_ _ _ -> return ()
, examplePending      = \_ _ _  -> return ()
, failedFormatter     = return ()
, footerFormatter     = return ()
}


specdoc :: Formatter
specdoc = silent {
  exampleGroupStarted = \nesting name -> do
    writeLine ("\n" ++ indentationForGroup nesting ++ name)

, exampleSucceeded = \nesting requirement -> withSuccessColor $ do
    writeLine $ indentationForExample nesting ++ " - " ++ requirement

, exampleFailed = \nesting requirement _ -> withFailColor $ do
    n <- getFailCount
    writeLine $ indentationForExample nesting ++ " - " ++ requirement ++ " FAILED [" ++ show n ++ "]"

, examplePending = \nesting requirement reason -> withPendingColor $ do
    writeLine $ indentationForExample nesting ++ " - " ++ requirement ++ "\n     # PENDING: " ++ fromMaybe "No reason given" reason

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationForExample nesting = replicate (pred (length nesting) * 2) ' '
    indentationForGroup nesting = replicate (length nesting * 2) ' '


progress :: Formatter
progress = silent {
  exampleSucceeded = \_ _ -> withSuccessColor $ write "."
, exampleFailed    = \_ _ _ -> withFailColor    $ write "F"
, examplePending   = \_ _ _ -> withPendingColor $ write "."
, failedFormatter  = defaultFailedFormatter
, footerFormatter  = defaultFooter
}


failed_examples :: Formatter
failed_examples   = silent {
  failedFormatter = defaultFailedFormatter
, footerFormatter = defaultFooter
}

defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = withFailColor $ do
  failures <- map formatFailure . zip [1..] <$> getFailMessages
  mapM_ writeLine ("" : intersperse "" failures)
  unless (null failures) (writeLine "")
  where
    formatFailure :: (Int, FailureRecord) -> String
    formatFailure (i, FailureRecord groups requirement reason) =
      show i ++ ") " ++ groups_ ++ requirement ++ " FAILED" ++ err
      where
        err = case reason of
          Left (E.SomeException e)  -> " (uncaught exception)\n" ++ (show . typeOf) e ++ " (" ++ show e ++ ")"
          Right e -> if null e then "" else "\n" ++ e
        groups_ = case groups of
          [x] -> x ++ " "
          _   -> concatMap (++ ", ") (reverse groups)

defaultFooter :: FormatM ()
defaultFooter = do

  writeLine =<< printf "Finished in %1.4f seconds, used %1.4f seconds of CPU time" <$> getRealTime <*> getCPUTime

  fails   <- getFailCount
  pending <- getPendingCount
  total   <- getTotalCount
  writeLine ""

  let c | fails /= 0   = withFailColor
        | pending /= 0 = withPendingColor
        | otherwise    = withSuccessColor
  c $ do
    write $ quantify total   "example"
    write (", " ++ show pending ++ " pending, ")
    write $ quantify fails "failure"
  writeLine ""
