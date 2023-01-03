-- |
-- Stability: stable
-- @since 2.10.9
module Test.Hspec.Api.Format.V1 (
  Format
, FormatConfig(..)
, Event(..)
, Progress
, Path
, Location(..)
, Seconds(..)
, Item(..)
, Result(..)
, FailureReason(..)
, monadic

-- * Register a formatter
, registerFormatter
, useFormatter
, liftFormatter

-- * Re-exports
, SpecWith
, Config
, modifyConfig
) where

import           Test.Hspec.Core.Runner (Config)
import           Test.Hspec.Core.Spec (modifyConfig, SpecWith)
import qualified Test.Hspec.Core.Runner as Runner
import           Test.Hspec.Core.Format hiding (FormatConfig(..))
import qualified Test.Hspec.Core.Format as Latest

-- |
-- Make a formatter available for use with @--format@.
registerFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
registerFormatter = Runner.registerFormatter . liftFormatter

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
useFormatter = Runner.registerDefaultFormatter . liftFormatter

-- | Make a formatter compatible with types from "Test.Hspec.Core.Format".
liftFormatter :: (String, FormatConfig -> IO Format) -> (String, Latest.FormatConfig -> IO Format)
liftFormatter = fmap liftFormat
  where
    liftFormat :: (FormatConfig -> IO Format) -> Latest.FormatConfig -> IO Format
    liftFormat format = format . liftFormatConfig

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigReportProgress :: Bool
, formatConfigOutputUnicode :: Bool
, formatConfigUseDiff :: Bool
, formatConfigDiffContext :: Maybe Int
, formatConfigExternalDiff :: Maybe (String -> String -> IO ())
, formatConfigPrettyPrintFunction :: Maybe (String -> String -> (String, String))
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigExpectedTotalCount :: Int
}

liftFormatConfig :: Latest.FormatConfig -> FormatConfig
liftFormatConfig (Latest.FormatConfig
  useColor
  reportProgress
  outputUnicode
  useDiff
  diffContext
  externalDiff
  _prettyPrint
  prettyPrintFunction
  printTimes
  htmlOutput
  printCpuTime
  usedSeed
  expectedTotalCount
  ) = FormatConfig {
    formatConfigUseColor = useColor
  , formatConfigReportProgress = reportProgress
  , formatConfigOutputUnicode = outputUnicode
  , formatConfigUseDiff = useDiff
  , formatConfigDiffContext = diffContext
  , formatConfigExternalDiff = externalDiff
  , formatConfigPrettyPrintFunction = prettyPrintFunction
  , formatConfigPrintTimes = printTimes
  , formatConfigHtmlOutput = htmlOutput
  , formatConfigPrintCpuTime = printCpuTime
  , formatConfigUsedSeed = usedSeed
  , formatConfigExpectedTotalCount = expectedTotalCount
  }
