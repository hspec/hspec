{-# LANGUAGE ViewPatterns #-}
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
module Test.Hspec.Api.Formatters.V3 (

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

import Test.Hspec.Core.Formatters.V2
import Test.Hspec.Core.Runner (Config(..))
import Test.Hspec.Core.Format (FormatConfig, Format)
import Test.Hspec.Core.Spec (modifyConfig, SpecWith)

-- |
-- Make a formatter available for use with @--format@.
registerFormatter :: (String, Formatter) -> Config -> Config
registerFormatter formatter = registerFormatter_ (fmap formatterToFormat formatter)

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, Formatter) -> Config -> Config
useFormatter (fmap formatterToFormat -> formatter@(_, format)) config = (registerFormatter_ formatter config) { configFormat = Just format }

-- copy of Test.Hspec.Core.Runner.registerFormatter
registerFormatter_ :: (String, FormatConfig -> IO Format) -> Config -> Config
registerFormatter_ formatter config = config { configAvailableFormatters = formatter : configAvailableFormatters config }
