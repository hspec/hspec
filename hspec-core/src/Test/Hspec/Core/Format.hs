{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Stability: experimental
module Test.Hspec.Core.Format (
  Format
, FormatConfig(..)
, Event(..)
, Progress
, Path
, Location(..)
, Seconds(..)
, Item(..)
, Result(..)
, FailureReason(..)
, monadic
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.Async (async)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class

import           Test.Hspec.Core.Spec (Progress, Location(..))
import           Test.Hspec.Core.Example (FailureReason(..))
import           Test.Hspec.Core.Util (Path)
import           Test.Hspec.Core.Clock (Seconds(..))

type Format = Event -> IO ()

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

data Event =
    Started
  | GroupStarted Path
  | GroupDone Path
  | Progress Path Progress
  | ItemStarted Path
  | ItemDone Path Item
  | Done [(Path, Item)]
  deriving Show

data FormatConfig = FormatConfig {
  formatConfigUseColor :: Bool
, formatConfigUseDiff :: Bool
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigItemCount :: Int
} deriving (Eq, Show)

data Signal = Ok | NotOk SomeException

monadic :: MonadIO m => (m () -> IO ()) -> (Event -> m ()) -> IO Format
monadic run format = do
  mvar <- newEmptyMVar
  done <- newEmptyMVar

  let
    putEvent :: Event -> IO ()
    putEvent = putMVar mvar

    takeEvent :: MonadIO m => m Event
    takeEvent = liftIO $ takeMVar mvar

    signal :: MonadIO m => Signal -> m ()
    signal = liftIO . putMVar done

    wait :: IO Signal
    wait = takeMVar done

    go = do
      event <- takeEvent
      format event
      case event of
        Done {} -> return ()
        _ -> do
          signal Ok
          go

  t <- async $ do
    (run go >> signal Ok) `catch` (signal . NotOk)

  return $ \ event -> do
    running <- Async.poll t
    case running of
      Just _ -> return ()
      Nothing -> do
        putEvent event
        r <- wait
        case r of
          Ok -> return ()
          NotOk err -> do
            Async.wait t
            throwIO err
