-- |
-- Stability: deprecated
--
-- This module contains formatters that can be used with `hspecWith`:
--
-- @
-- import Test.Hspec
-- import Test.Hspec.Api.Formatters.V1
--
-- main :: IO ()
-- main = hspecWith (setFormatter formatter defaultConfig) spec
--
-- formatter :: Formatter
-- formatter = ...
--
-- spec :: Spec
-- spec = ...
-- @
module Test.Hspec.Api.Formatters.V1 (

  setFormatter
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

-- * Re-exports
, Location(..)
, Progress
) where

import Test.Hspec.Core.Formatters.V1
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec

setFormatter :: Formatter -> Config -> Config
setFormatter formatter config = config { configFormat = Just $ formatterToFormat formatter }
