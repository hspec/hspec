{-# LANGUAGE RankNTypes #-}
module Test.Hspec.Core.Format (
  Format(..)
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

data Format m = Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatItemStarted :: Path -> m ()
, formatItemDone :: Path -> Item -> m ()
}
