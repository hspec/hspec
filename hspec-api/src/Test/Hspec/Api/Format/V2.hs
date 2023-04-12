{-# LANGUAGE ViewPatterns #-}
-- |
-- Stability: stable
module Test.Hspec.Api.Format.V2 (
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

import           Test.Hspec.Core.Runner (Config(..))
import           Test.Hspec.Core.Spec (modifyConfig, SpecWith)
import           Test.Hspec.Core.Format hiding (FormatConfig(..))
import qualified Test.Hspec.Core.Format as Latest

-- |
-- Make a formatter available for use with @--format@.
registerFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
registerFormatter = registerFormatter_ . liftFormatter

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
useFormatter (liftFormatter -> formatter@(_, format)) config = (registerFormatter_ formatter config) { configFormat = Just format }

-- copy of Test.Hspec.Core.Runner.registerFormatter
registerFormatter_ :: (String, Latest.FormatConfig -> IO Latest.Format) -> Config -> Config
registerFormatter_ formatter config = config { configAvailableFormatters = formatter : configAvailableFormatters config }

-- | Make a formatter compatible with types from "Test.Hspec.Core.Format".
liftFormatter :: (String, FormatConfig -> IO Format) -> (String, Latest.FormatConfig -> IO Format)
liftFormatter = fmap liftFormat
  where
    liftFormat :: (FormatConfig -> IO Format) -> Latest.FormatConfig -> IO Format
    liftFormat format = format . unliftFormatConfig

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

unliftFormatConfig :: Latest.FormatConfig -> FormatConfig
unliftFormatConfig config = FormatConfig {
  formatConfigUseColor = Latest.formatConfigUseColor config
, formatConfigReportProgress = Latest.formatConfigReportProgress config
, formatConfigOutputUnicode = Latest.formatConfigOutputUnicode config
, formatConfigUseDiff = Latest.formatConfigUseDiff config
, formatConfigDiffContext = Latest.formatConfigDiffContext config
, formatConfigExternalDiff = Latest.formatConfigExternalDiff config
, formatConfigPrettyPrintFunction = Latest.formatConfigPrettyPrintFunction config
, formatConfigPrintTimes = Latest.formatConfigPrintTimes config
, formatConfigHtmlOutput = Latest.formatConfigHtmlOutput config
, formatConfigPrintCpuTime = Latest.formatConfigPrintCpuTime config
, formatConfigUsedSeed = Latest.formatConfigUsedSeed config
, formatConfigExpectedTotalCount = Latest.formatConfigExpectedTotalCount config
}
