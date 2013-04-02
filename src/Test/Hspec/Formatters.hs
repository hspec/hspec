-- |
-- Stability: experimental
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Runner.hspecWith`.
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
, usedSeed

, getCPUTime
, getRealTime

-- ** Appending to the gerenated report
, write
, writeLine
, newParagraph

-- ** Dealing with colors
, withSuccessColor
, withPendingColor
, withFailColor

-- ** Helpers
, formatException
) where

import           Data.Maybe
import           Test.Hspec.Util
import           Test.Hspec.Compat
import           Text.Printf
import           Control.Monad (unless, forM_)
import           Control.Applicative
import qualified Control.Exception as E
import           System.IO (hPutStr)

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
  , usedSeed

  , getCPUTime
  , getRealTime

  , write
  , writeLine
  , newParagraph

  , withSuccessColor
  , withPendingColor
  , withFailColor
  )


silent :: Formatter
silent = Formatter {
  headerFormatter     = return ()
, exampleGroupStarted = \_ _ _ -> return ()
, exampleGroupDone    = return ()
, exampleProgress     = \_ _ _ -> return ()
, exampleSucceeded    = \_ -> return ()
, exampleFailed       = \_ _ -> return ()
, examplePending      = \_ _  -> return ()
, failedFormatter     = return ()
, footerFormatter     = return ()
}


specdoc :: Formatter
specdoc = silent {

  headerFormatter = do
    writeLine ""

, exampleGroupStarted = \n nesting name -> do

    -- separate groups with an empty line
    unless (n == 0) $ do
      newParagraph

    writeLine (indentationFor nesting ++ name)

, exampleGroupDone = do
    newParagraph

, exampleProgress = \h _ (current, total) -> do
    hPutStr h $ "(" ++ show current ++ "/" ++ show total ++ ")\r"

, exampleSucceeded = \(nesting, requirement) -> withSuccessColor $ do
    writeLine $ indentationFor nesting ++ "- " ++ requirement

, exampleFailed = \(nesting, requirement) _ -> withFailColor $ do
    n <- getFailCount
    writeLine $ indentationFor nesting ++ "- " ++ requirement ++ " FAILED [" ++ show n ++ "]"

, examplePending = \(nesting, requirement) reason -> withPendingColor $ do
    writeLine $ indentationFor nesting ++ "- " ++ requirement ++ "\n     # PENDING: " ++ fromMaybe "No reason given" reason

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '


progress :: Formatter
progress = silent {
  exampleSucceeded = \_   -> withSuccessColor $ write "."
, exampleFailed    = \_ _ -> withFailColor    $ write "F"
, examplePending   = \_ _ -> withPendingColor $ write "."
, failedFormatter  = defaultFailedFormatter
, footerFormatter  = defaultFooter
}


failed_examples :: Formatter
failed_examples   = silent {
  failedFormatter = defaultFailedFormatter
, footerFormatter = defaultFooter
}

defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = do
  newParagraph

  failures <- getFailMessages

  forM_ (zip [1..] failures) $ \x -> do
    formatFailure x
    writeLine ""
  unless (null failures) $ do
    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    formatFailure :: (Int, FailureRecord) -> FormatM ()
    formatFailure (n, FailureRecord path reason) = do
      write (show n ++ ") ")
      withFailColor $ do
        write (formatRequirement path ++ " FAILED")
        write $ case reason of
          Right _ -> "\n"
          Left _  -> " (uncaught exception)\n"
        unless (null err) $ do
          writeLine err
      where
        err = either formatException id reason

-- | Convert an exception to a string.
--
-- The type of the exception is included.  Here is an example:
--
-- >>> import Control.Applicative
-- >>> import Control.Exception
-- >>> either formatException show <$> (try . evaluate) (1 `div` 0)
-- "ArithException (divide by zero)"
formatException :: E.SomeException -> String
formatException (E.SomeException e) = showType e ++ " (" ++ show e ++ ")"

defaultFooter :: FormatM ()
defaultFooter = do

  writeLine =<< (++)
    <$> (printf "Finished in %1.4f seconds"
    <$> getRealTime) <*> (maybe "" (printf ", used %1.4f seconds of CPU time") <$> getCPUTime)

  fails   <- getFailCount
  pending <- getPendingCount
  total   <- getTotalCount

  let c | fails /= 0   = withFailColor
        | pending /= 0 = withPendingColor
        | otherwise    = withSuccessColor
  c $ do
    write $ quantify total   "example"
    write (", " ++ quantify fails "failure")
    unless (pending == 0) $
      write (", " ++ show pending ++ " pending")
  writeLine ""
