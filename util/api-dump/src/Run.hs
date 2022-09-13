{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (
  main
#ifdef TEST
, dumpModuleApi
#endif
) where

import           Prelude hiding (writeFile)
import qualified Prelude

import           Control.Monad
import           Control.Exception
import           System.Exit
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.Process

import           Hpack.Config

import qualified Format

main :: IO ()
main = do
  args <- getArgs
  format <- case args of
    [] -> return Format.format
    ["--dump-only"] -> return id
    _ -> die "usage: api-dump [--dump-only]"
  modules <- filter (/= "Test.Hspec.Discover") <$> exposedModules
  forM_ modules $ \ module_@(Module name) -> do
    dumpModuleApi format module_ >>= writeFile ("api" </> name)

writeFile :: FilePath -> String -> IO ()
writeFile name contents = do
  createDirectoryIfMissing True (takeDirectory name)
  Prelude.writeFile name contents

dumpModuleApi :: (String -> String) -> Module -> IO String
dumpModuleApi format module_ = browse module_ >>= removePackageVersions . format

browse :: Module -> IO String
browse (Module module_) = do
  readProcess "cabal" ["-v0", "exec", "--", "ghci", "-v0", "-ignore-dot-ghci"] $ ":m " <> module_ <> "\n:browse " <> module_

removePackageVersions :: String -> IO String
removePackageVersions = readProcess "sed" ["s/-[[:digit:]]\\(\\.[[:digit:]]\\+\\)\\+:/:/g"]

exposedModules :: IO [Module]
exposedModules = readPackageConfig defaultDecodeOptions >>= \ case
  Left err -> throwIO $ ErrorCall err
  Right result -> return $ modules result
  where
    modules :: DecodeResult -> [Module]
    modules = maybe [] (libraryExposedModules . sectionData) . packageLibrary . decodeResultPackage
