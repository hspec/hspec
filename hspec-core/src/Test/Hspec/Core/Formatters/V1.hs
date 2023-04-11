-- |
-- Stability: deprecated
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Core.Runner.hspecWith`.
module Test.Hspec.Core.Formatters.V1
{-# DEPRECATED "Use [Test.Hspec.Api.Formatters.V1](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Formatters-V1.html) instead." #-}
(
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
import           Test.Hspec.Core.Formatters.V1.Internal
