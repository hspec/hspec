{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Test.Hspec.Contrib.LoggingFacadeSpec (main, spec) where

import Test.Hspec
import Data.IORef
import System.Logging.Facade.Types
import System.Logging.Facade.Sink
import System.Logging.Facade as Log

import Test.Hspec.Contrib.LoggingFacade

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Hspec.Contrib.LoggingFacade" $ do
  describe "captureLogs" $ do
    let logToIORef :: IORef [LogRecord] -> LogSink
        logToIORef ref record = modifyIORef ref (record :)

    it "returns all logs of an action" $ do
      (logs, ()) <- captureLogs $ do
        Log.trace "this should be captured"
        Log.trace "this should be captured next"
      logs `shouldBe` [ LogRecord TRACE Nothing "this should be captured"
                      , LogRecord TRACE Nothing "this should be captured next"
                      ]

    it "prevents logs from going to the log sink" $ do
      ref <- newIORef []
      setLogSink $ logToIORef ref
      _ <- captureLogs $ Log.trace "this should be captured"
      readIORef ref `shouldReturn` []
