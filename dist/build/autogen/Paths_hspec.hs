module Paths_hspec (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\Trystan\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Trystan\\AppData\\Roaming\\cabal\\hspec-0.1.0\\ghc-6.12.3"
datadir    = "C:\\Users\\Trystan\\AppData\\Roaming\\cabal\\hspec-0.1.0"
libexecdir = "C:\\Users\\Trystan\\AppData\\Roaming\\cabal\\hspec-0.1.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "hspec_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "hspec_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "hspec_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "hspec_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
