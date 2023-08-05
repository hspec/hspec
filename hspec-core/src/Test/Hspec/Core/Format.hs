{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Stability: unstable
--
-- This is an unstable API.  Use
-- [Test.Hspec.Api.Format.V2](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Format-V2.html)
-- instead.
module Test.Hspec.Core.Format
-- {-# WARNING "Use [Test.Hspec.Api.Format.V2](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Format-V2.html) instead." #-}
(
  Format
, FormatConfig(..)
, defaultFormatConfig
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
import           Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class

import           Test.Hspec.Core.Example (Progress, Location(..), FailureReason(..))
import           Test.Hspec.Core.Util (Path, formatExceptionWith)
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
, formatConfigReportProgress :: Bool
, formatConfigOutputUnicode :: Bool
, formatConfigUseDiff :: Bool
, formatConfigDiffContext :: Maybe Int
, formatConfigExternalDiff :: Maybe (String -> String -> IO ())
, formatConfigPrettyPrint :: Bool
, formatConfigPrettyPrintFunction :: Maybe (String -> String -> (String, String))
, formatConfigFormatException :: SomeException -> String -- ^ @since 2.11.5
, formatConfigPrintTimes :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
, formatConfigExpectedTotalCount :: Int
, formatConfigExpertMode :: Bool
}

{-# DEPRECATED formatConfigPrettyPrint "Use `formatConfigPrettyPrintFunction` instead" #-}

-- ^ @since 2.11.5
defaultFormatConfig :: FormatConfig
defaultFormatConfig = FormatConfig {
  formatConfigUseColor = False
, formatConfigReportProgress = False
, formatConfigOutputUnicode = False
, formatConfigUseDiff = False
, formatConfigDiffContext = Nothing
, formatConfigExternalDiff = Nothing
, formatConfigPrettyPrint = False
, formatConfigPrettyPrintFunction = Nothing
, formatConfigFormatException = formatExceptionWith show
, formatConfigPrintTimes = False
, formatConfigHtmlOutput = False
, formatConfigPrintCpuTime = False
, formatConfigUsedSeed = 0
, formatConfigExpectedTotalCount = 0
, formatConfigExpertMode = False
}

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
        Done {} -> pass
        _ -> do
          signal Ok
          go

  worker <- async $ do
    (run go >> signal Ok) `catch` (signal . NotOk)

  return $ \ event -> do
    running <- asyncRunning worker
    when running $ do
      putEvent event
      r <- wait
      case r of
        Ok -> pass
        NotOk err -> do
          Async.wait worker
          throwIO err

asyncRunning :: Async () -> IO Bool
asyncRunning worker = maybe True (const False) <$> Async.poll worker
