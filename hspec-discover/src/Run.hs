{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
module Run (
  run

-- exported for testing
, Spec(..)
, importList
, fileToSpec
, findSpecs
, getFilesRecursive
, driverWithFormatter
, moduleNameFromId
, pathToModule
) where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Maybe
import           Data.String
import           System.Environment
import           System.Exit
import           System.IO
import           System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import           System.FilePath hiding (combine)

import           Config

instance IsString ShowS where
  fromString = showString

data Spec = Spec {
  specFile :: FilePath
, specModule :: String
} deriving (Eq, Show)

run :: [String] -> IO ()
run args_ = do
  name <- getProgName
  case args_ of
    src : _ : dst : args -> case parseConfig name args of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right conf -> do
        let modName = moduleName src conf
            futureModName = futureModuleName src conf

            warn :: [String] -> IO ()
            warn xs = hPutStrLn stderr ("\nhspec-discover: WARNING in " ++ src ++ "\n" ++ msg)
              where
                msg = unlines $ map ("    " ++) xs
        when (configNested conf) (warn ["The `--nested' flag is deprecated and will be removed with a future version of hspec-discover!"])
        when (modName /= futureModName) $ do
          let suggestSrc = replaceFileName src (modName ++ takeExtension src)
              suggestFlag = "--module-name=" ++ modName
          warn [
              "Filename '" ++ src ++ "' does not match module name '" ++ modName ++ "'."
            , "This will lead to breakage with future versions of hspec-discover!"
            , ""
            , "You can either rename '" ++ src ++ "' to '" ++ suggestSrc ++ "'"
            , ""
            , "    mv '" ++ src ++ "' '" ++ suggestSrc ++ "'"
            , ""
            , "or pass `" ++ suggestFlag ++ "' to hspec-discover"
            , ""
            , "    echo '{-# OPTIONS_GHC -F -pgmF hspec-discover -optF " ++ suggestFlag ++ " #-}' > " ++ src
            , ""
            , "to prevent future breakage."
            ]
        specs <- findSpecs src
        writeFile dst (mkSpecModule src conf specs)
    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure

mkSpecModule :: FilePath -> Config -> [Spec] -> String
mkSpecModule src conf nodes =
  ( "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString ("module " ++ moduleName src conf ++" where\n")
  . importList nodes
  . showString "import Test.Hspec.Discover\n"
  . maybe driver driverWithFormatter (configFormatter conf)
  . showString "spec :: Spec\n"
  . showString "spec = "
  . formatSpecs nodes
  ) "\n"
  where
    driver =
        case configNoMain conf of
          False ->
              showString "main :: IO ()\n"
            . showString "main = hspec spec\n"
          True -> ""

futureModuleName :: FilePath -> Config -> String
futureModuleName src conf = fromMaybe (pathToModule src) (configModuleName conf)

moduleName :: FilePath -> Config -> String
moduleName src conf = fromMaybe (if configNoMain conf then pathToModule src else "Main") (configModuleName conf)

-- | Derive module name from specified path.
pathToModule :: FilePath -> String
pathToModule f = toUpper m:ms
  where
    fileName = last $ splitDirectories f
    m:ms = takeWhile (/='.') fileName

driverWithFormatter :: String -> ShowS
driverWithFormatter f =
    showString "import qualified " . showString (moduleNameFromId f) . showString "\n"
  . showString "main :: IO ()\n"
  . showString "main = hspecWithFormatter " . showString f . showString " spec\n"

-- | Return module name of a fully qualified identifier.
moduleNameFromId :: String -> String
moduleNameFromId = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse

-- | Generate imports for a list of specs.
importList :: [Spec] -> ShowS
importList = foldr (.) "" . map f
  where
    f :: Spec -> ShowS
    f spec = "import qualified " . showString (specModule spec) . "Spec\n"

-- | Combine a list of strings with (>>).
sequenceS :: [ShowS] -> ShowS
sequenceS = foldr (.) "" . intersperse " >> "

-- | Convert a list of specs to code.
formatSpecs :: [Spec] -> ShowS
formatSpecs xs
  | null xs   = "return ()"
  | otherwise = sequenceS (map formatSpec xs)

-- | Convert a spec to code.
formatSpec :: Spec -> ShowS
formatSpec (Spec file name) = "postProcessSpec " . shows file . " (describe " . shows name . " " . showString name . "Spec.spec)"

findSpecs :: FilePath -> IO [Spec]
findSpecs src = do
  let (dir, file) = splitFileName src
  mapMaybe (fileToSpec dir) . filter (/= file) <$> getFilesRecursive dir

fileToSpec :: FilePath -> FilePath -> Maybe Spec
fileToSpec dir file = case reverse $ splitDirectories file of
  x:xs -> case stripSuffix "Spec.hs" x <|> stripSuffix "Spec.lhs" x of
    Just name | (not . null) name -> Just . Spec (dir </> file) $ (intercalate "." . reverse) (name : xs)
    _ -> Nothing
  _ -> Nothing
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)
