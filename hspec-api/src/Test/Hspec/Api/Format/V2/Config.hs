module Test.Hspec.Api.Format.V2.Config where

import           Control.Exception (SomeException)
import qualified Test.Hspec.Core.Format as Latest

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigReportProgress :: Bool
, formatConfigOutputUnicode :: Bool
, formatConfigUseDiff :: Bool
, formatConfigDiffContext :: Maybe Int
, formatConfigExternalDiff :: Maybe (String -> String -> IO ())
, formatConfigPrettyPrintFunction :: Maybe (String -> String -> (String, String))
, formatConfigFormatException :: SomeException -> String -- ^ @since 2.11.5
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigExpectedTotalCount :: Int
}

-- ^ @since 2.11.5
defaultFormatConfig :: FormatConfig
defaultFormatConfig = unliftFormatConfig Latest.defaultFormatConfig

unliftFormatConfig :: Latest.FormatConfig -> FormatConfig
unliftFormatConfig config = FormatConfig {
  formatConfigUseColor = Latest.formatConfigUseColor config
, formatConfigReportProgress = Latest.formatConfigReportProgress config
, formatConfigOutputUnicode = Latest.formatConfigOutputUnicode config
, formatConfigUseDiff = Latest.formatConfigUseDiff config
, formatConfigDiffContext = Latest.formatConfigDiffContext config
, formatConfigExternalDiff = Latest.formatConfigExternalDiff config
, formatConfigPrettyPrintFunction = Latest.formatConfigPrettyPrintFunction config
, formatConfigFormatException = Latest.formatConfigFormatException config
, formatConfigPrintTimes = Latest.formatConfigPrintTimes config
, formatConfigHtmlOutput = Latest.formatConfigHtmlOutput config
, formatConfigPrintCpuTime = Latest.formatConfigPrintCpuTime config
, formatConfigUsedSeed = Latest.formatConfigUsedSeed config
, formatConfigExpectedTotalCount = Latest.formatConfigExpectedTotalCount config
}
