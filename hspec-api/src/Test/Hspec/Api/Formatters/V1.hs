{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
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
-- main = hspecWith (useFormatter ("my-formatter", formatter) defaultConfig) spec
--
-- formatter :: Formatter
-- formatter = ...
--
-- spec :: Spec
-- spec = ...
-- @
module Test.Hspec.Api.Formatters.V1 (

-- * Register a formatter
  useFormatter
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

, Config
) where

import Test.Hspec.Core.Formatters.V1
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Format

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, Formatter) -> Config -> Config
useFormatter (fmap formatterToFormat -> formatter@(_, format)) config = (registerFormatter_ formatter config) { configFormat = Just format }

-- copy of Test.Hspec.Core.Runner.registerFormatter
registerFormatter_ :: (String, FormatConfig -> IO Format) -> Config -> Config
#if MIN_VERSION_hspec_core(2,9,0)
registerFormatter_ formatter config = config { configAvailableFormatters = formatter : configAvailableFormatters config }
#else
registerFormatter_ _ config = config
#endif
