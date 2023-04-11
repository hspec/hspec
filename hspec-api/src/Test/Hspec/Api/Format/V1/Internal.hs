{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.Hspec.Api.Format.V1.Internal (
  FailureReason(..)
, module Test.Hspec.Api.Format.V1.Internal
) where

import           Test.Hspec.Core.Util (stripAnsi)
import           Test.Hspec.Core.Formatters.V1 (FailureReason(..))
import qualified Test.Hspec.Api.Format.V2 as V2
import           Test.Hspec.Api.Format.V2 hiding (Item(..), Result(..), FailureReason(..))

data Item = Item {
  itemLocation :: Maybe Location
, itemDuration :: Seconds
, itemInfo :: String
, itemResult :: Result
} deriving Show

data Result =
    Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving Show

liftItem :: Item -> V2.Item
liftItem Item{..} = V2.Item {
  itemLocation
, itemDuration
, itemInfo
, itemResult = liftResult itemResult
}

unliftItem :: V2.Item -> Item
unliftItem V2.Item{..} = Item {
  itemLocation
, itemDuration
, itemInfo
, itemResult = unliftResult itemResult
}

liftResult :: Result -> V2.Result
liftResult = \ case
  Success -> V2.Success
  Pending loc reason -> V2.Pending loc reason
  Failure loc reason -> V2.Failure loc (liftFailureReson reason)

unliftResult :: V2.Result -> Result
unliftResult = \ case
  V2.Success -> Success
  V2.Pending loc reason -> Pending loc reason
  V2.Failure loc reason -> Failure loc (unliftFailureReason reason)

liftFailureReson :: FailureReason -> V2.FailureReason
liftFailureReson = \ case
  NoReason -> V2.NoReason
  Reason reason -> V2.Reason reason
  ExpectedButGot preface expected actual -> V2.ExpectedButGot preface expected actual
  Error info e -> V2.Error info e

unliftFailureReason :: V2.FailureReason -> FailureReason
unliftFailureReason = \ case
  V2.NoReason -> NoReason
  V2.Reason reason -> Reason reason
  V2.ColorizedReason reason -> Reason (stripAnsi reason)
  V2.ExpectedButGot preface expected actual -> ExpectedButGot preface expected actual
  V2.Error info e -> Error info e
