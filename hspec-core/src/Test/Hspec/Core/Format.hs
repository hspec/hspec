{-# LANGUAGE RankNTypes #-}
module Test.Hspec.Core.Format (
  Format(..)
) where

import           Control.Exception

import           Test.Hspec.Core.Spec (Progress, Location)
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)

data Format m = Format {
  formatRun :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> Progress -> m ()
, formatSuccess :: Path -> m ()
, formatFailure :: Path -> Maybe Location -> Either SomeException FailureReason -> m ()
, formatPending :: Path -> Maybe String -> m ()
}
