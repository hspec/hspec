module Helper (
  module Test.Hspec.Meta
, withFileContent
) where

import           Control.Exception (finally)
import           System.Directory
import           System.IO
import           Test.Hspec.Meta

import           Test.Hspec.QuickCheck ()

withFileContent :: String -> (FilePath -> IO a) -> IO a
withFileContent input action = do
  dir <- getTemporaryDirectory
  (file, h) <- openTempFile dir "temp"
  hPutStr h input
  hClose h
  action file `finally` removeFile file
