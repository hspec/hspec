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
, TextBlockHandle
, TextBlock(..)
, module Test.Hspec.Core.Formatters.TextBlock

, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, getCPUTime
, getRealTime

, insertTextBlock
, writeTextBlock
, writeLine
, rewrite

, useDiff

, Environment(..)
, interpretWith
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.IO.Class

import GHC.Exts (IsString(..))

import           Test.Hspec.Core.Formatters.Free
import           Test.Hspec.Core.Formatters.TextBlock
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Spec (Progress, Location)
import           Test.Hspec.Core.Clock

data Formatter = Formatter {

  headerFormatter :: FormatM ()

-- | evaluated before each test group
, exampleGroupStarted :: [String] -> String -> FormatM ()

, exampleGroupDone :: [String] -> String -> FormatM ()

-- | used to notify the progress of the currently evaluated example
--
-- /Note/: This is only called when interactive/color mode.
, exampleProgress :: Path -> Progress -> FormatM ()

-- | evaluated when an example is started
, exampleStarted   :: Path -> FormatM ()

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

-- | If given, called at the end with the captured program output
--   If not given the program output is not captured
, capturedOutputFormatter :: Maybe (String -> FormatM ())
}

data FailureRecord = FailureRecord {
  failureRecordLocation :: Maybe Location
, failureRecordPath     :: Path
, failureRecordMessage  :: FailureReason
}

newtype TextBlockHandle = TextBlockHandle Int

data FormatF next =
    GetSuccessCount (Int -> next)
  | GetPendingCount (Int -> next)
  | GetFailMessages ([FailureRecord] -> next)
  | UsedSeed (Integer -> next)
  | GetCPUTime (Maybe Seconds -> next)
  | GetRealTime (Seconds -> next)
  | Write (TextBlock ()) (TextBlockHandle -> next)
  | Insert  TextBlockHandle (TextBlock ()) (TextBlockHandle -> next)
  | Rewrite TextBlockHandle (TextBlock () -> Maybe (TextBlock ())) next
  | UseDiff (Bool -> next)
  | forall a. LiftIO (IO a) (a -> next)

instance Functor FormatF where -- deriving this instance would require GHC >= 7.10.1
  fmap f x = case x of
    GetSuccessCount next -> GetSuccessCount (fmap f next)
    GetPendingCount next -> GetPendingCount (fmap f next)
    GetFailMessages next -> GetFailMessages (fmap f next)
    UsedSeed next -> UsedSeed (fmap f next)
    GetCPUTime next -> GetCPUTime (fmap f next)
    GetRealTime next -> GetRealTime (fmap f next)
    Write s next -> Write s (fmap f next)
    Insert l s next -> Insert l s (fmap f next)
    Rewrite l s next -> Rewrite l s (f next)
    UseDiff next -> UseDiff (fmap f next)
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
, environmentWrite :: TextBlock () -> m Int
, environmentInsert :: Int -> TextBlock () -> m Int
, environmentRewrite :: Int -> (TextBlock () -> Maybe(TextBlock ())) -> m ()
, environmentUseDiff :: m Bool
, environmentLiftIO :: forall a. IO a -> m a
}

interpretWith :: forall m a. Monad m => Environment m -> FormatM a -> m a
interpretWith Environment{..} = foldF alg return
  where
    alg action = case action of
        GetSuccessCount next -> environmentGetSuccessCount >>= next
        GetPendingCount next -> environmentGetPendingCount >>= next
        GetFailMessages next -> environmentGetFailMessages >>= next
        UsedSeed next -> environmentUsedSeed >>=  next
        GetCPUTime next -> environmentGetCPUTime >>= next
        GetRealTime next -> environmentGetRealTime >>=  next
        Write s next -> environmentWrite s >>= next . TextBlockHandle
        Insert (TextBlockHandle l) s next -> environmentInsert l s >>= next . TextBlockHandle
        Rewrite (TextBlockHandle l) s next -> environmentRewrite l s >> next
        UseDiff next -> environmentUseDiff >>= next
        LiftIO inner next -> environmentLiftIO inner >>=  next

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

-- | Insert a text block to the report.
writeTextBlock :: TextBlock () -> FormatM TextBlockHandle
writeTextBlock s = liftF (Write s id)

-- | Insert a text block below the given handle
insertTextBlock :: TextBlockHandle -> TextBlock () -> FormatM TextBlockHandle
insertTextBlock l s = liftF (Insert l s id)

-- | Insert some output to the report, adding a newline if not already terminated in one.
writeLine :: String -> FormatM ()
writeLine = void . writeTextBlock . fromString

-- | Modify or remove a text block from the report
rewrite :: TextBlockHandle -> (TextBlock () -> Maybe(TextBlock ())) -> FormatM ()
rewrite l s = liftF (Rewrite l s ())

-- | Return `True` if the user requested colorized diffs, `False` otherwise.
useDiff :: FormatM Bool
useDiff = liftF (UseDiff id)
