{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Stability: stable
--
-- This module contains formatters that can be used with `hspecWith`:
--
-- @
-- import Test.Hspec
-- import Test.Hspec.Api.Formatters.V1
--
-- main :: IO ()
-- main = hspecWith (useFormatter ("my-formatter", formatter) defaultConfig) spec
--
-- formatter :: Formatter
-- formatter = ...
--
-- spec :: Spec
-- spec = ...
-- @
module Test.Hspec.Api.Formatters.V2 (

-- * Register a formatter
  registerFormatter
, useFormatter
, formatterToFormat

-- * Formatters
, silent
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
, Path
, Progress
, Location(..)
, Item(..)
, Result(..)
, FailureReason (..)
, FormatM

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount
, getExpectedTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, printTimes

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

, outputUnicode

, useDiff
, diffContext
, externalDiffAction
, prettyPrint
, prettyPrintFunction
, extraChunk
, missingChunk

-- ** Helpers
, formatLocation
, formatException

-- * Re-exports
, SpecWith
, Config
, modifyConfig
) where

import           Test.Hspec.Core.Format (FormatConfig, Format)
import           Test.Hspec.Core.Formatters.V1 (FailureRecord(..))
import           Test.Hspec.Api.Format.V1.Internal
import qualified Test.Hspec.Api.Formatters.V3 as V3
import           Test.Hspec.Api.Formatters.V3 hiding (
    registerFormatter
  , useFormatter
  , formatterToFormat

  , FormatConfig

  , silent
  , checks
  , specdoc
  , progress
  , failed_examples

  , Formatter(..)
  , Item(..)
  , Result(..)
  , FailureReason(..)

  , FailureRecord(..)
  , getFailMessages
  )

-- |
-- Make a formatter available for use with @--format@.
registerFormatter :: (String, Formatter) -> Config -> Config
registerFormatter formatter = V3.registerFormatter (fmap liftFormatter formatter)

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, Formatter) -> Config -> Config
useFormatter formatter = V3.useFormatter (liftFormatter <$> formatter)

formatterToFormat :: Formatter -> FormatConfig -> IO Format
formatterToFormat = V3.formatterToFormat . liftFormatter

silent :: Formatter
silent = unliftFormatter V3.silent

checks :: Formatter
checks = unliftFormatter V3.checks

specdoc :: Formatter
specdoc = unliftFormatter V3.specdoc

progress :: Formatter
progress = unliftFormatter V3.progress

failed_examples :: Formatter
failed_examples = unliftFormatter V3.failed_examples

data Formatter = Formatter {
-- | evaluated before a test run
  formatterStarted :: FormatM ()

-- | evaluated before each spec group
, formatterGroupStarted :: Path -> FormatM ()

-- | evaluated after each spec group
, formatterGroupDone :: Path -> FormatM ()

-- | used to notify the progress of the currently evaluated example
, formatterProgress :: Path -> Progress -> FormatM ()

-- | evaluated before each spec item
, formatterItemStarted :: Path -> FormatM ()

-- | evaluated after each spec item
, formatterItemDone :: Path -> Item -> FormatM ()

-- | evaluated after a test run
, formatterDone :: FormatM ()
}

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = map unliftFailureRecord <$> V3.getFailMessages

liftFormatter :: Formatter -> V3.Formatter
liftFormatter Formatter{..} = V3.Formatter{
  formatterStarted
, formatterGroupStarted
, formatterGroupDone
, formatterProgress
, formatterItemStarted
, formatterItemDone = \ path -> formatterItemDone path . unliftItem
, formatterDone
}

unliftFormatter :: V3.Formatter -> Formatter
unliftFormatter V3.Formatter{..} = Formatter{
  formatterStarted
, formatterGroupStarted
, formatterGroupDone
, formatterProgress
, formatterItemStarted
, formatterItemDone = \ path -> formatterItemDone path . liftItem
, formatterDone
}

unliftFailureRecord :: V3.FailureRecord -> FailureRecord
unliftFailureRecord V3.FailureRecord{..} = FailureRecord {
  failureRecordLocation
, failureRecordPath
, failureRecordMessage = unliftFailureReason failureRecordMessage
}
