{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import           Control.Concurrent
import           Control.Concurrent.Async (async)
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

monadic :: MonadIO m => (m () -> IO ()) -> (Event -> m ()) -> IO Format
monadic run format = do
  mvar <- newEmptyMVar
  done <- newEmptyMVar

  let
    putEvent :: Event -> IO ()
    putEvent = putMVar mvar

    takeEvent :: MonadIO m => m Event
    takeEvent = liftIO $ takeMVar mvar

    signal :: MonadIO m => m ()
    signal = liftIO $ putMVar done ()

    wait :: IO ()
    wait = takeMVar done

    go = do
      event <- takeEvent
      format event
      case event of
        Done {} -> return ()
        _ -> do
          signal
          go

  _ <- async $ do
    run go
    signal

  return $ \ event -> do
    putEvent event
    wait
