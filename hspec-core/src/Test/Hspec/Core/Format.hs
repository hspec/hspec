{-# LANGUAGE RankNTypes #-}
module Test.Hspec.Core.Format (
  Format(..)
, Progress
, Path
, Location(..)
, Item(..)
, Result(..)
, FailureReason(..)
) where

import           Control.Exception

import           Test.Hspec.Core.Spec (Progress, Location(..))
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)

data Item = Item {
  itemLocation :: Maybe Location
, itemResult :: Result
}

data Result =
    Success String
  | Pending (Maybe String)
  | Failure (Either SomeException FailureReason)

data Format m = Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatItem :: Path -> Item -> m ()
}
