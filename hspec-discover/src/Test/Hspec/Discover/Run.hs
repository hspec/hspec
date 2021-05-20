{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
--
-- /NOTE:/ This module is not meant for public consumption.  For user
-- documentation look at http://hspec.github.io/hspec-discover.html.
module Test.Hspec.Discover.Run (
  run

-- exported for testing
, Spec(..)
, driverWithFormatter
, moduleNameFromId
, pathToModule
, Tree(..)
, Forest(..)
, SpecForest(..)
, Hook(..)
, WithConfig(..)
, discover
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

import           Test.Hspec.Discover.Config
import           Test.Hspec.Discover.Sort

instance IsString ShowS where
  fromString = showString

data Spec = Spec String | Hook String [Spec]
  deriving (Eq, Show)

data Specs = Specs {
  specsConfig :: WithConfig
, _specsList :: [Spec]
}

run :: [String] -> IO ()
run args_ = do
  name <- getProgName
  case args_ of
    src : _ : dst : args -> case parseConfig name args of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right conf -> do
        when (configNested conf)             (hPutStrLn stderr "hspec-discover: WARNING - The `--nested' option is deprecated and will be removed in a future release!")
        when (configNoMain conf)             (hPutStrLn stderr "hspec-discover: WARNING - The `--no-main' option is deprecated and will be removed in a future release!")
        when (isJust $ configFormatter conf) (hPutStrLn stderr "hspec-discover: WARNING - The `--formatter' option is deprecated and will be removed in a future release!")
        specs <- findSpecs src
        writeFile dst (mkSpecModule src conf specs)
    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure

mkSpecModule :: FilePath -> Config -> Maybe Specs -> String
mkSpecModule src conf nodes =
  ( "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# LANGUAGE NoImplicitPrelude #-}\n"
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
    config :: WithConfig
    config = maybe WithoutConfig specsConfig nodes

    driver =
        case configNoMain conf of
          False -> case config of
            WithConfig ->
                showString "main :: IO ()\n"
              . showString "main = SpecConfig.config defaultConfig >>= flip hspecWith spec\n"
            WithoutConfig ->
                showString "main :: IO ()\n"
              . showString "main = hspec spec\n"
          True -> ""

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
importList :: Maybe Specs -> ShowS
importList = foldr (.) "" . map f . maybe [] moduleNames
  where
    f :: String -> ShowS
    f spec = "import qualified " . showString spec . "\n"

moduleNames :: Specs -> [String]
moduleNames (Specs config specs) = withConfig $ fromForest specs
  where
    withConfig = case config of
      WithConfig -> ("SpecConfig" :)
      WithoutConfig -> id

    fromForest :: [Spec] -> [String]
    fromForest = concatMap fromTree

    fromTree :: Spec -> [String]
    fromTree tree = case tree of
      Spec name -> [name ++ "Spec"]
      Hook name forest -> name : fromForest forest

-- | Combine a list of strings with (>>).
sequenceS :: [ShowS] -> ShowS
sequenceS = foldr (.) "" . intersperse " >> "

formatSpecs :: Maybe Specs -> ShowS
formatSpecs specs = case specs of
  Nothing -> "return ()"
  Just (Specs _ xs) -> fromForest xs
  where
    fromForest :: [Spec] -> ShowS
    fromForest = sequenceS . map fromTree

    fromTree :: Spec -> ShowS
    fromTree tree = case tree of
      Spec name -> "describe " . shows name . " " . showString name . "Spec.spec"
      Hook name forest -> "(" . showString name . ".hook $ " . fromForest forest . ")"

findSpecs :: FilePath -> IO (Maybe Specs)
findSpecs = fmap (fmap toSpecs) . discover

toSpecs :: SpecForest -> Specs
toSpecs (SpecForest config specs) = Specs config $ fromForest [] specs
  where
    fromForest :: [String] -> Forest -> [Spec]
    fromForest names (Forest WithHook xs) = [Hook (mkModule ("SpecHook" : names)) $ concatMap (fromTree names) xs]
    fromForest names (Forest WithoutHook xs) = concatMap (fromTree names) xs

    fromTree :: [String] -> Tree -> [Spec]
    fromTree names spec = case spec of
      Leaf name -> [Spec $ mkModule (name : names )]
      Node name forest -> fromForest (name : names) forest

    mkModule :: [String] -> String
    mkModule = intercalate "." . reverse

-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

data Tree = Leaf String | Node String Forest
  deriving (Eq, Show)

data Forest = Forest Hook [Tree]
  deriving (Eq, Show)

data SpecForest = SpecForest WithConfig Forest
  deriving (Eq, Show)

data Hook = WithHook | WithoutHook
  deriving (Eq, Show)

data WithConfig = WithConfig | WithoutConfig
  deriving (Eq, Show)

sortKey :: Tree -> (String, Int)
sortKey tree = case tree of
  Leaf name -> (name, 0)
  Node name _ -> (name, 1)

discover :: FilePath -> IO (Maybe SpecForest)
discover src = fmap (uncurry SpecForest) . (>>= filterSrc) <$> specForest dir
  where
    filterSrc :: (WithConfig, Forest) -> Maybe (WithConfig, Forest)
    filterSrc (config, Forest hook xs) = ensureForest config hook $ maybe id (filter . (/=)) (toSpec file) xs

    (dir, file) = splitFileName src

specForest :: FilePath -> IO (Maybe (WithConfig, Forest))
specForest dir = do
  files <- listDirectory dir

  hook <- bool_ WithoutHook WithHook <$> hasFile "SpecHook.hs" dir files
  config <- bool_ WithoutConfig WithConfig <$> hasFile "SpecConfig.hs" dir files

  ensureForest config hook . sortNaturallyBy sortKey . catMaybes <$> mapM toSpecTree files
  where
    toSpecTree :: FilePath -> IO (Maybe Tree)
    toSpecTree name
      | isValidModuleName name = do
          doesDirectoryExist (dir </> name) `fallback` Nothing $ do
            xs <- fmap snd <$> specForest (dir </> name)
            return $ Node name <$> xs
      | otherwise = do
          doesFileExist (dir </> name) `fallback` Nothing $ do
            return $ toSpec name

hasFile :: FilePath -> FilePath -> [FilePath] -> IO Bool
hasFile file dir files
  | file `elem` files = doesFileExist (dir </> file)
  | otherwise = return False

fallback :: IO Bool -> a -> IO a -> IO a
fallback p def action = do
  bool <- p
  if bool then action else return def

toSpec :: FilePath -> Maybe Tree
toSpec file = Leaf <$> (spec >>= ensure isValidModuleName)
  where
    spec :: Maybe String
    spec = stripSuffix "Spec.hs" file <|> stripSuffix "Spec.lhs" file

    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

ensure :: (a -> Bool) -> a -> Maybe a
ensure p a = guard (p a) >> Just a

ensureForest :: WithConfig -> Hook -> [Tree] -> Maybe (WithConfig, Forest)
ensureForest config hook = fmap ((,) config . Forest hook) . ensure (not . null)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

bool_ :: a -> a -> Bool -> a
bool_ f _ False = f
bool_ _ t True  = t
