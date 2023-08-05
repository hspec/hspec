{-# LANGUAGE ViewPatterns #-}
-- |
-- Stability: stable
module Test.Hspec.Api.Format.V2 (
  Format
, FormatConfig(..)
, defaultFormatConfig
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
import           Test.Hspec.Core.Format hiding (FormatConfig(..), defaultFormatConfig)
import qualified Test.Hspec.Core.Format as Latest

import           Test.Hspec.Api.Format.V2.Config

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
