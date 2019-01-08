{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Hspec.Core.Golden
  ( Golden(..)
  , GoldenResult(..)
  , defaultGolden
  , runGolden
  )
  where

import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  )


-- | Golden tests parameters

data Golden str =
  forall. Eq str =>
    Golden {
      output :: str, -- ^ Lazy bytestring output
      writeToFile :: FilePath -> str -> IO (), -- ^ How to write into the golden file the file
      readFromFile :: FilePath -> IO str, -- ^ How to read the file,
      testName :: String, -- ^ Test name (make sure it's unique otherwise it could be override)
      directory :: FilePath -- ^ Directory where you write your tests
    }

-- | An example of Golden tests which output is 'String'

defaultGolden :: String -> String -> Golden String
defaultGolden name output_ =
  Golden {
    output = output_,
    testName = name,
    writeToFile = writeFile,
    readFromFile = readFile,
    directory = ".hspec"
  }

-- | Possible results from a golden test execution

data GoldenResult =
   MissmatchOutput
   | SameOutput
   | FirstExecution

-- | Runs a Golden test.

runGolden :: Golden str -> IO GoldenResult
runGolden Golden{..} =
  let goldenTestDir = directory ++ "/" ++ testName
      goldenFilePath = goldenTestDir ++ "/" ++ "golden"
      actualFilePath = goldenTestDir ++ "/" ++ "actual"
   in do
     createDirectoryIfMissing True goldenTestDir
     goldenFileExist <- doesFileExist goldenFilePath
     actualFileExist <- doesFileExist actualFilePath
     case (goldenFileExist, actualFileExist) of
       (False, _)    -> writeToFile goldenFilePath output
                             >> return FirstExecution
       (True, False) -> do
          contentGolden <- readFromFile goldenFilePath
          if contentGolden == output
             then return SameOutput
             else writeToFile actualFilePath output
                    >> return MissmatchOutput

       (True, True) -> do
          contentGolden <- readFromFile goldenFilePath
          contentActual <- readFromFile actualFilePath
          if contentGolden == contentActual
             then return SameOutput
             else writeToFile actualFilePath output
                    >> return MissmatchOutput
