{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Hspec.Core.Formatters.Monad (
  Formatter (..)
, FailureReason (..)
, FormatM

, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, getCPUTime
, getRealTime

, write
, writeLine
, writeTransient

, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, useDiff
, extraChunk
, missingChunk

, Environment(..)
, interpretWith
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.IO.Class

import           Test.Hspec.Core.Formatters.Free
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Spec (Progress, Location)
import           Test.Hspec.Core.Clock

data Formatter = Formatter {

  headerFormatter :: FormatM ()

-- | evaluated before each test group
, exampleGroupStarted :: [String] -> String -> FormatM ()

, exampleGroupDone :: FormatM ()

-- | used to notify the progress of the currently evaluated example
--
-- /Note/: This is only called when interactive/color mode.
, exampleProgress :: Path -> Progress -> FormatM ()

-- | evaluated after each successful example
, exampleSucceeded :: Path -> String -> FormatM ()

-- | evaluated after each failed example
, exampleFailed :: Path -> String -> FailureReason -> FormatM ()

-- | evaluated after each pending example
, examplePending :: Path -> String -> Maybe String -> FormatM ()

-- | evaluated after a test run
, failedFormatter :: FormatM ()

-- | evaluated after `failuresFormatter`
, footerFormatter :: FormatM ()
}

data FailureRecord = FailureRecord {
  failureRecordLocation :: Maybe Location
, failureRecordPath     :: Path
, failureRecordMessage  :: FailureReason
}

data FormatF next =
    GetSuccessCount (Int -> next)
  | GetPendingCount (Int -> next)
  | GetFailMessages ([FailureRecord] -> next)
  | UsedSeed (Integer -> next)
  | GetCPUTime (Maybe Seconds -> next)
  | GetRealTime (Seconds -> next)
  | Write String next
  | WriteTransient String next
  | forall a. WithFailColor (FormatM a) (a -> next)
  | forall a. WithSuccessColor (FormatM a) (a -> next)
  | forall a. WithPendingColor (FormatM a) (a -> next)
  | forall a. WithInfoColor (FormatM a) (a -> next)
  | UseDiff (Bool -> next)
  | ExtraChunk String next
  | MissingChunk String next
  | forall a. LiftIO (IO a) (a -> next)

instance Functor FormatF where -- deriving this instance would require GHC >= 7.10.1
  fmap f x = case x of
    GetSuccessCount next -> GetSuccessCount (fmap f next)
    GetPendingCount next -> GetPendingCount (fmap f next)
    GetFailMessages next -> GetFailMessages (fmap f next)
    UsedSeed next -> UsedSeed (fmap f next)
    GetCPUTime next -> GetCPUTime (fmap f next)
    GetRealTime next -> GetRealTime (fmap f next)
    Write s next -> Write s (f next)
    WriteTransient s next -> WriteTransient s (f next)
    WithFailColor action next -> WithFailColor action (fmap f next)
    WithSuccessColor action next -> WithSuccessColor action (fmap f next)
    WithPendingColor action next -> WithPendingColor action (fmap f next)
    WithInfoColor action next -> WithInfoColor action (fmap f next)
    UseDiff next -> UseDiff (fmap f next)
    ExtraChunk s next -> ExtraChunk s (f next)
    MissingChunk s next -> MissingChunk s (f next)
    LiftIO action next -> LiftIO action (fmap f next)

type FormatM = Free FormatF

instance MonadIO FormatM where
  liftIO s = liftF (LiftIO s id)

data Environment m = Environment {
  environmentGetSuccessCount :: m Int
, environmentGetPendingCount :: m Int
, environmentGetFailMessages :: m [FailureRecord]
, environmentUsedSeed :: m Integer
, environmentGetCPUTime :: m (Maybe Seconds)
, environmentGetRealTime :: m Seconds
, environmentWrite :: String -> m ()
, environmentWriteTransient :: String -> m ()
, environmentWithFailColor :: forall a. m a -> m a
, environmentWithSuccessColor :: forall a. m a -> m a
, environmentWithPendingColor :: forall a. m a -> m a
, environmentWithInfoColor :: forall a. m a -> m a
, environmentUseDiff :: m Bool
, environmentExtraChunk :: String -> m ()
, environmentMissingChunk :: String -> m ()
, environmentLiftIO :: forall a. IO a -> m a
}

interpretWith :: forall m a. Monad m => Environment m -> FormatM a -> m a
interpretWith Environment{..} = go
  where
    go :: forall b. FormatM b -> m b
    go m = case m of
      Pure value -> return value
      Free action -> case action of
        GetSuccessCount next -> environmentGetSuccessCount >>= go . next
        GetPendingCount next -> environmentGetPendingCount >>= go . next
        GetFailMessages next -> environmentGetFailMessages >>= go . next
        UsedSeed next -> environmentUsedSeed >>= go . next
        GetCPUTime next -> environmentGetCPUTime >>= go . next
        GetRealTime next -> environmentGetRealTime >>= go . next
        Write s next -> environmentWrite s >> go next
        WriteTransient s next -> environmentWriteTransient s >> go next
        WithFailColor inner next -> environmentWithFailColor (go inner) >>= go . next
        WithSuccessColor inner next -> environmentWithSuccessColor (go inner) >>= go . next
        WithPendingColor inner next -> environmentWithPendingColor (go inner) >>= go . next
        WithInfoColor inner next -> environmentWithInfoColor (go inner) >>= go . next
        UseDiff next -> environmentUseDiff >>= go . next
        ExtraChunk s next -> environmentExtraChunk s >> go next
        MissingChunk s next -> environmentMissingChunk s >> go next
        LiftIO inner next -> environmentLiftIO inner >>= go . next

-- | Get the number of successful examples encountered so far.
getSuccessCount :: FormatM Int
getSuccessCount = liftF (GetSuccessCount id)

-- | Get the number of pending examples encountered so far.
getPendingCount :: FormatM Int
getPendingCount = liftF (GetPendingCount id)

-- | Get the number of failed examples encountered so far.
getFailCount :: FormatM Int
getFailCount = length <$> getFailMessages

-- | Get the total number of examples encountered so far.
getTotalCount :: FormatM Int
getTotalCount = sum <$> sequence [getSuccessCount, getFailCount, getPendingCount]

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = liftF (GetFailMessages id)

-- | The random seed that is used for QuickCheck.
usedSeed :: FormatM Integer
usedSeed = liftF (UsedSeed id)

-- | Get the used CPU time since the test run has been started.
getCPUTime :: FormatM (Maybe Seconds)
getCPUTime = liftF (GetCPUTime id)

-- | Get the passed real time since the test run has been started.
getRealTime :: FormatM Seconds
getRealTime = liftF (GetRealTime id)

-- | Append some output to the report.
write :: String -> FormatM ()
write s = liftF (Write s ())

-- | The same as `write`, but adds a newline character.
writeLine :: String -> FormatM ()
writeLine s = write s >> write "\n"

writeTransient :: String -> FormatM ()
writeTransient s = liftF (WriteTransient s ())

-- | Set output color to red, run given action, and finally restore the default
-- color.
withFailColor :: FormatM a -> FormatM a
withFailColor s = liftF (WithFailColor s id)

-- | Set output color to green, run given action, and finally restore the
-- default color.
withSuccessColor :: FormatM a -> FormatM a
withSuccessColor s = liftF (WithSuccessColor s id)

-- | Set output color to yellow, run given action, and finally restore the
-- default color.
withPendingColor :: FormatM a -> FormatM a
withPendingColor s = liftF (WithPendingColor s id)

-- | Set output color to cyan, run given action, and finally restore the
-- default color.
withInfoColor :: FormatM a -> FormatM a
withInfoColor s = liftF (WithInfoColor s id)

-- | Return `True` if the user requested colorized diffs, `False` otherwise.
useDiff :: FormatM Bool
useDiff = liftF (UseDiff id)

-- | Output given chunk in red.
extraChunk :: String -> FormatM ()
extraChunk s = liftF (ExtraChunk s ())

-- | Output given chunk in green.
missingChunk :: String -> FormatM ()
missingChunk s = liftF (MissingChunk s ())
