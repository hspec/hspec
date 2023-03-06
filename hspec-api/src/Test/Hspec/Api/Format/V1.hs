{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Stability: stable
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
, useFormatter
, liftFormatter
) where

import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Format hiding (FormatConfig(..))
import qualified Test.Hspec.Core.Format as Latest

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
useFormatter (liftFormatter -> formatter@(_, format)) config = (registerFormatter_ formatter config) { configFormat = Just format }

-- copy of Test.Hspec.Core.Runner.registerFormatter
registerFormatter_ :: (String, Latest.FormatConfig -> IO Latest.Format) -> Config -> Config
#if MIN_VERSION_hspec_core(2,10,9)
registerFormatter_ formatter config = config { configAvailableFormatters = formatter : configAvailableFormatters config }
#else
registerFormatter_ _ config = config
#endif

-- | Make a formatter compatible with types from "Test.Hspec.Core.Format".
liftFormatter :: (String, FormatConfig -> IO Format) -> (String, Latest.FormatConfig -> IO Format)
liftFormatter = fmap liftFormat
  where
    liftFormat :: (FormatConfig -> IO Format) -> Latest.FormatConfig -> IO Format
    liftFormat format = format . liftFormatConfig

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigUseDiff :: Bool
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigExpectedTotalCount :: Int
}

liftFormatConfig :: Latest.FormatConfig -> FormatConfig
liftFormatConfig config = FormatConfig {
  formatConfigUseColor = Latest.formatConfigUseColor config
, formatConfigUseDiff = Latest.formatConfigUseDiff config
, formatConfigPrintTimes = Latest.formatConfigPrintTimes config
, formatConfigHtmlOutput = Latest.formatConfigHtmlOutput config
, formatConfigPrintCpuTime = Latest.formatConfigPrintCpuTime config
, formatConfigUsedSeed = Latest.formatConfigUsedSeed config
#if MIN_VERSION_hspec_core(2,9,0)
, formatConfigExpectedTotalCount = Latest.formatConfigExpectedTotalCount config
#else
, formatConfigExpectedTotalCount = Latest.formatConfigItemCount config
#endif
}
