{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Test.Hspec.Core.Format (
  Format(..)
, SomeFormat(..)
, FormatConfig(..)
, IsFormatter(..)
, LifeCycle(..)
, Progress
, Path
, Location(..)
, Seconds(..)
, Item(..)
, Result(..)
, FailureReason(..)
) where

import           Prelude ()
import           Control.Monad.IO.Class
import           System.IO (Handle)
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Spec (Location(..))
import           Test.Hspec.Core.Example (FailureReason(..), LifeCycle(..), Progress)
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
  formatRun  :: forall a. m a -> IO a
, formatGroupStarted :: Path -> m ()
, formatGroupDone :: Path -> m ()
, formatProgress :: Path -> LifeCycle Progress -> m ()
, formatItem :: Path -> Item -> m ()
  -- | Whether the formatter wants to be notified asynchronously
, formatAsynchronously :: Bool
}

-- TODO Rename to Formatter
data SomeFormat where
  SomeFormat :: (Applicative m, MonadIO m) => (FormatConfig -> IO (Format m)) -> SomeFormat

data FormatConfig = FormatConfig {
  formatConfigHandle :: Handle
, formatConfigUseColor :: Bool
, formatConfigUseDiff :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
} deriving (Eq, Show)

class IsFormatter a where
  toFormatter :: a -> IO SomeFormat

instance IsFormatter (IO SomeFormat) where
  toFormatter = id

instance IsFormatter SomeFormat where
  toFormatter = return

instance (Applicative m, MonadIO m) => IsFormatter (Format m) where
  toFormatter = return . SomeFormat . const . return

instance (Applicative m, MonadIO m) => IsFormatter (IO (Format m)) where
  toFormatter = return . SomeFormat . const

instance (Applicative m, MonadIO m) => IsFormatter (FormatConfig -> Format m) where
  toFormatter = return . SomeFormat . fmap return

instance (Applicative m, MonadIO m) => IsFormatter (FormatConfig -> IO (Format m)) where
  toFormatter = return . SomeFormat

instance (Applicative m, MonadIO m) => IsFormatter (IO (FormatConfig -> Format m)) where
  toFormatter = fmap (SomeFormat . fmap return)

instance (Applicative m, MonadIO m) => IsFormatter (IO (FormatConfig -> IO (Format m))) where
  toFormatter = fmap SomeFormat
