{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Stability: deprecated
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Core.Runner.hspecWith`.
module Test.Hspec.Core.Formatters.V1 (

-- * Formatters
  silent
, checks
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
, formatterToFormat

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
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Spec (Location(..))
import           Text.Printf
import           Control.Monad.IO.Class
import           Control.Exception

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.Monad (
    FailureReason (..)
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

import           Test.Hspec.Core.Spec (Progress)
import           Test.Hspec.Core.Format (FormatConfig, Format, Item(..), Result(..))
import qualified Test.Hspec.Core.Formatters.V2 as V2

import           Test.Hspec.Core.Formatters.Diff

formatterToFormat :: Formatter -> FormatConfig -> IO Format
formatterToFormat Formatter{..} = V2.formatterToFormat V2.Formatter {
  V2.formatterStarted = headerFormatter
, V2.formatterGroupStarted = uncurry exampleGroupStarted
, V2.formatterGroupDone = \ _ -> exampleGroupDone
, V2.formatterProgress = exampleProgress
, V2.formatterItemStarted = exampleStarted
, V2.formatterItemDone = \ path item -> do
    case itemResult item of
      Success -> exampleSucceeded path (itemInfo item)
      Pending _ reason -> examplePending path (itemInfo item) reason
      Failure _ reason -> exampleFailed path (itemInfo item) reason
, V2.formatterDone = failedFormatter >> footerFormatter
}

data Formatter = Formatter {

  headerFormatter :: FormatM ()

-- | evaluated before each test group
, exampleGroupStarted :: [String] -> String -> FormatM ()

-- | evaluated after each test group
, exampleGroupDone :: FormatM ()

-- | evaluated before each example
, exampleStarted :: Path -> FormatM ()

-- | used to notify the progress of the currently evaluated example
, exampleProgress :: Path -> Progress -> FormatM ()

-- | evaluated after each successful example
, exampleSucceeded :: Path -> String -> FormatM ()

-- | evaluated after each failed example
, exampleFailed :: Path -> String -> FailureReason -> FormatM ()

-- | evaluated after each pending example
, examplePending :: Path -> String -> Maybe String -> FormatM ()

-- | evaluated after a test run
, failedFormatter :: FormatM ()

-- | evaluated after `failedFormatter`
, footerFormatter :: FormatM ()
}

silent :: Formatter
silent = Formatter {
  headerFormatter     = return ()
, exampleGroupStarted = \_ _ -> return ()
, exampleGroupDone    = return ()
, exampleStarted      = \_ -> return ()
, exampleProgress     = \_ _ -> return ()
, exampleSucceeded    = \ _ _ -> return ()
, exampleFailed       = \_ _ _ -> return ()
, examplePending      = \_ _ _ -> return ()
, failedFormatter     = return ()
, footerFormatter     = return ()
}

checks :: Formatter
checks = specdoc {
  exampleStarted = \(nesting, requirement) -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [ ]"

, exampleProgress = \(nesting, requirement) p -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [" ++ (formatProgress p) ++ "]"

, exampleSucceeded = \(nesting, requirement) info -> do
    writeResult nesting requirement info $ withSuccessColor $ write "✔"

, exampleFailed = \(nesting, requirement) info _ -> do
    writeResult nesting requirement info $ withFailColor $ write "✘"

, examplePending = \(nesting, requirement) info reason -> do
    writeResult nesting requirement info $ withPendingColor $ write "‐"

    withPendingColor $ do
      writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult :: [String] -> String -> String -> FormatM () -> FormatM ()
    writeResult nesting requirement info action = do
      write $ indentationFor nesting ++ requirement ++ " ["
      action
      writeLine "]"
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

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

          let threshold = 2 :: Seconds

          mchunks <- liftIO $ if b
            then timeout threshold (evaluate $ diff expected actual)
            else return Nothing

          case mchunks of
            Just chunks -> do
              writeDiff chunks extraChunk missingChunk
            Nothing -> do
              writeDiff [First expected, Second actual] write write
          where
            indented output text = case break (== '\n') text of
              (xs, "") -> output xs
              (xs, _ : ys) -> output (xs ++ "\n") >> write (indentation ++ "          ") >> indented output ys

            writeDiff chunks extra missing = do
              withFailColor $ write (indentation ++ "expected: ")
              forM_ chunks $ \ chunk -> case chunk of
                Both a _ -> indented write a
                First a -> indented extra a
                Second _ -> return ()
              writeLine ""

              withFailColor $ write (indentation ++ " but got: ")
              forM_ chunks $ \ chunk -> case chunk of
                Both a _ -> indented write a
                First _ -> return ()
                Second a -> indented missing a
              writeLine ""

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
