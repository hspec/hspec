{-# LANGUAGE CPP #-}
-- |
-- Stability: experimental
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Runner.hspecWith`.
module Test.Hspec.Core.Formatters (

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
, FailureReason (..)
, FormatM

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, Seconds(..)
, getCPUTime
, getRealTime

-- ** Appending to the generated report
, write
, writeLine
, writeTransient

-- ** Dealing with colors
, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, useDiff
, extraChunk
, missingChunk

-- ** Helpers
, formatException
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (First)

import           Data.Maybe
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Location(..))
import           Text.Printf

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.Monad (
    Formatter (..)
  , FailureReason (..)
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
  , writeTransient

  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor

  , useDiff
  , extraChunk
  , missingChunk
  )

import           Test.Hspec.Core.Clock (Seconds(..))

import           Test.Hspec.Core.Formatters.Diff

silent :: Formatter
silent = Formatter {
  headerFormatter     = return ()
, exampleGroupStarted = \_ _ -> return ()
, exampleGroupDone    = return ()
, exampleProgress     = \_ _ -> return ()
, exampleSucceeded    = \ _ _ -> return ()
, exampleFailed       = \_ _ _ -> return ()
, examplePending      = \_ _ _ -> return ()
, failedFormatter     = return ()
, footerFormatter     = return ()
}

specdoc :: Formatter
specdoc = silent {

  headerFormatter = do
    writeLine ""

, exampleGroupStarted = \nesting name -> do
    writeLine (indentationFor nesting ++ name)

, exampleProgress = \_ p -> do
    writeTransient (formatProgress p)

, exampleSucceeded = \(nesting, requirement) info -> withSuccessColor $ do
    writeLine $ indentationFor nesting ++ requirement
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s

, exampleFailed = \(nesting, requirement) info _ -> withFailColor $ do
    n <- getFailCount
    writeLine $ indentationFor nesting ++ requirement ++ " FAILED [" ++ show n ++ "]"
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s

, examplePending = \(nesting, requirement) info reason -> withPendingColor $ do
    writeLine $ indentationFor nesting ++ requirement
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s
    writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '
    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total


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
defaultFailedFormatter = do
  writeLine ""

  failures <- getFailMessages

  unless (null failures) $ do
    writeLine "Failures:"
    writeLine ""

    forM_ (zip [1..] failures) $ \x -> do
      formatFailure x
      writeLine ""

#if __GLASGOW_HASKELL__ == 800
    withFailColor $ do
      writeLine "WARNING:"
      writeLine "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
      writeLine "  Source locations may not work as expected."
      writeLine ""
      writeLine "  Please consider upgrading GHC!"
      writeLine ""
#endif

    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    formatFailure :: (Int, FailureRecord) -> FormatM ()
    formatFailure (n, FailureRecord mLoc path reason) = do
      forM_ mLoc $ \loc -> do
        withInfoColor $ writeLine (formatLoc loc)
      write ("  " ++ show n ++ ") ")
      writeLine (formatRequirement path)
      case reason of
        NoReason -> return ()
        Reason err -> withFailColor $ indent err
        ExpectedButGot preface expected actual -> do
          mapM_ indent preface

          b <- useDiff
          let
            chunks
              | b = diff expected actual
              | otherwise = [First expected, Second actual]

          withFailColor $ write (indentation ++ "expected: ")
          forM_ chunks $ \chunk -> case chunk of
            Both a _ -> indented write a
            First a -> indented extraChunk a
            Second _ -> return ()
          writeLine ""

          withFailColor $ write (indentation ++ " but got: ")
          forM_ chunks $ \chunk -> case chunk of
            Both a _ -> indented write a
            First _ -> return ()
            Second a -> indented missingChunk a
          writeLine ""
          where
            indented output text = case break (== '\n') text of
              (xs, "") -> output xs
              (xs, _ : ys) -> output (xs ++ "\n") >> write (indentation ++ "          ") >> indented output ys
        Error _ e -> withFailColor . indent $ (("uncaught exception: " ++) . formatException) e

      writeLine ""
      writeLine ("  To rerun use: --match " ++ show (joinPath path))
      where
        indentation = "       "
        indent message = do
          forM_ (lines message) $ \line -> do
            writeLine (indentation ++ line)
        formatLoc (Location file line column) = "  " ++ file ++ ":" ++ show line ++ ":" ++ show column ++ ": "

defaultFooter :: FormatM ()
defaultFooter = do

  writeLine =<< (++)
    <$> (printf "Finished in %1.4f seconds" <$> getRealTime)
    <*> (maybe "" (printf ", used %1.4f seconds of CPU time") <$> getCPUTime)

  fails   <- getFailCount
  pending <- getPendingCount
  total   <- getTotalCount

  let
    output =
         pluralize total   "example"
      ++ ", " ++ pluralize fails "failure"
      ++ if pending == 0 then "" else ", " ++ show pending ++ " pending"
    c | fails /= 0   = withFailColor
      | pending /= 0 = withPendingColor
      | otherwise    = withSuccessColor
  c $ writeLine output
