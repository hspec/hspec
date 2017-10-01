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

import           Test.Hspec.Core.Spec (Progress, Location(..))
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Clock

data Item = Item {
  itemLocation :: Maybe Location
, itemDuration :: Seconds
, itemInfo :: String
, itemResult :: Result
}

data Result =
    Success
  | Pending (Maybe String)
  | Failure FailureReason

data Format m = Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatItem :: Path -> Item -> m ()
}
