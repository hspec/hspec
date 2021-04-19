{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Hspec.Core.Format (
  Format(..)
, FormatConfig(..)
, Progress
, Path
, Location(..)
, Seconds(..)
, Item(..)
, Result(..)
, FailureReason(..)
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.IO.Class

import           Test.Hspec.Core.Spec (Progress, Location(..))
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Clock

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

data Format = forall m. (Functor m, Applicative m, MonadIO m) => Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatItemStarted :: Path -> m ()
, formatItemDone :: Path -> Item -> m ()
}

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigUseDiff :: Bool
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigItemCount :: Int
} deriving (Eq, Show)
