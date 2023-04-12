{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
, registerFormatter
, useFormatter
, liftFormatter

-- * Re-exports
, SpecWith
, Config
, modifyConfig
) where

import qualified Test.Hspec.Core.Format as Latest
import qualified Test.Hspec.Api.Format.V2 as V2
import           Test.Hspec.Api.Format.V2 hiding (
    registerFormatter
  , useFormatter
  , liftFormatter
  , FormatConfig(..)
  , Item(..)
  , FailureReason(..)
  , Result(..)
  , Event(..)
  , Format
  , monadic
  )

import           Control.Monad.IO.Class
import           Test.Hspec.Api.Format.V1.Internal

-- |
-- Make a formatter available for use with @--format@.
registerFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
registerFormatter = V2.registerFormatter . liftFormatterToV2

-- |
-- Make a formatter available for use with @--format@ and use it by default.
useFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
useFormatter = V2.useFormatter . liftFormatterToV2

-- | Make a formatter compatible with types from "Test.Hspec.Core.Format".
liftFormatter :: (String, FormatConfig -> IO Format) -> (String, Latest.FormatConfig -> IO Latest.Format)
liftFormatter = V2.liftFormatter . liftFormatterToV2

liftFormatterToV2 :: (String, FormatConfig -> IO Format) -> (String, V2.FormatConfig -> IO V2.Format)
liftFormatterToV2 = fmap lift
  where
    lift :: (FormatConfig -> IO Format) -> V2.FormatConfig -> IO V2.Format
    lift format = fmap liftFormat . format . unliftFormatConfig

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigReportProgress :: Bool
, formatConfigOutputUnicode :: Bool
, formatConfigUseDiff :: Bool
, formatConfigDiffContext :: Maybe Int
, formatConfigExternalDiff :: Maybe (String -> String -> IO ())
, formatConfigPrettyPrint :: Bool -- ^ Deprecated: use `formatConfigPrettyPrintFunction` instead
, formatConfigPrettyPrintFunction :: Maybe (String -> String -> (String, String))
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigExpectedTotalCount :: Int
}

unliftFormatConfig :: V2.FormatConfig -> FormatConfig
unliftFormatConfig config = FormatConfig {
  formatConfigUseColor = V2.formatConfigUseColor config
, formatConfigReportProgress = V2.formatConfigReportProgress config
, formatConfigOutputUnicode = V2.formatConfigOutputUnicode config
, formatConfigUseDiff = V2.formatConfigUseDiff config
, formatConfigDiffContext = V2.formatConfigDiffContext config
, formatConfigExternalDiff = V2.formatConfigExternalDiff config
, formatConfigPrettyPrint = maybe False (const True) $ V2.formatConfigPrettyPrintFunction config
, formatConfigPrettyPrintFunction = V2.formatConfigPrettyPrintFunction config
, formatConfigPrintTimes = V2.formatConfigPrintTimes config
, formatConfigHtmlOutput = V2.formatConfigHtmlOutput config
, formatConfigPrintCpuTime = V2.formatConfigPrintCpuTime config
, formatConfigUsedSeed = V2.formatConfigUsedSeed config
, formatConfigExpectedTotalCount = V2.formatConfigExpectedTotalCount config
}

type Format = Event -> IO ()

liftFormat :: Format -> V2.Format
liftFormat format event = format (unliftEvent event)

unliftFormat :: V2.Format -> Format
unliftFormat format event = format (liftEvent event)

data Event =
    Started
  | GroupStarted Path
  | GroupDone Path
  | Progress Path Progress
  | ItemStarted Path
  | ItemDone Path Item
  | Done [(Path, Item)]
  deriving Show

liftEvent :: Event -> V2.Event
liftEvent = \ case
  Started -> V2.Started
  GroupStarted path -> V2.GroupStarted path
  GroupDone path -> V2.GroupDone path
  Progress path progress -> V2.Progress path progress
  ItemStarted path -> V2.ItemStarted path
  ItemDone path item -> V2.ItemDone path (liftItem item)
  Done result -> V2.Done (map (fmap liftItem) result)

unliftEvent :: V2.Event -> Event
unliftEvent = \ case
  V2.Started -> Started
  V2.GroupStarted path -> GroupStarted path
  V2.GroupDone path -> GroupDone path
  V2.Progress path progress -> Progress path progress
  V2.ItemStarted path -> ItemStarted path
  V2.ItemDone path item -> ItemDone path (unliftItem item)
  V2.Done result -> Done (map (fmap unliftItem) result)

monadic :: MonadIO m => (m () -> IO ()) -> (Event -> m ()) -> IO Format
monadic run format = unliftFormat <$> V2.monadic run (format . unliftEvent)
