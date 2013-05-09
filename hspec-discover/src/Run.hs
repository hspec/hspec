{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
module Run where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Function
import           System.Environment
import           System.Exit
import           System.IO
import           System.Directory
import           System.FilePath hiding (combine)

import           Config

instance IsString ShowS where
  fromString = showString

run :: [String] -> IO ()
run args_ = do
  name <- getProgName
  case args_ of
    src : _ : dst : args -> case parseConfig name args of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right c -> do
        specs <- findSpecs src
        writeFile dst (mkSpecModule src c specs)
    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure

mkSpecModule :: FilePath -> Config -> [SpecNode] -> String
mkSpecModule src c nodes =
  ( "{-# LINE 1 " . shows src . " #-}"
  . showString "module Main where\n"
  . importList nodes
  . maybe driver (driverWithFormatter (null nodes)) (configFormatter c)
  . format nodes
  ) "\n"
  where
    format
      | configNested c = formatSpecsNested
      | otherwise = formatSpecs

    driver =
        showString "import Test.Hspec\n"
      . showString "main :: IO ()\n"
      . showString "main = hspec $ "

driverWithFormatter :: Bool -> String -> ShowS
driverWithFormatter isEmpty f =
    (if isEmpty then id else "import Test.Hspec\n")
  . showString "import Test.Hspec.Runner\n"
  . showString "import qualified " . showString (moduleName f) . showString "\n"
  . showString "main :: IO ()\n"
  . showString "main = hspecWith defaultConfig "
  . showString "{configFormatter = " . showString f . showString "} $ "

moduleName :: String -> String
moduleName = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse

-- | Generate imports for a list of specs.
importList :: [SpecNode] -> ShowS
importList = go ""
  where
    go :: ShowS -> [SpecNode] -> ShowS
    go current = foldr (.) "" . map (f current)
    f current (SpecNode name inhabited children) = this . go (current . showString name . ".") children
      where
        this
          | inhabited = "import qualified " . current . showString name . "Spec\n"
          | otherwise = id

-- | Combine a list of strings with (>>).
sequenceS :: [ShowS] -> ShowS
sequenceS = foldr (.) "" . intersperse " >> "

-- | Convert a list of specs to code.
formatSpecs :: [SpecNode] -> ShowS
formatSpecs xs
  | null xs   = "return ()"
  | otherwise = sequenceS (map formatSpec xs)

-- | Convert a spec to code.
formatSpec :: SpecNode -> ShowS
formatSpec = sequenceS . go ""
  where
    go :: String -> SpecNode -> [ShowS]
    go current (SpecNode name inhabited children) = addThis $ concatMap (go (current ++ name ++ ".")) children
      where
        addThis :: [ShowS] -> [ShowS]
        addThis
          | inhabited = ("describe " . shows (current ++ name) . " " . showString (current ++ name ++ "Spec.spec") :)
          | otherwise = id

-- | Convert a list of specs to code.
--
-- Hierarchical modules are mapped to nested specs.
formatSpecsNested :: [SpecNode] -> ShowS
formatSpecsNested xs
  | null xs   = "return ()"
  | otherwise = sequenceS (map formatSpecNested xs)

-- | Convert a spec to code.
--
-- Hierarchical modules are mapped to nested specs.
formatSpecNested :: SpecNode -> ShowS
formatSpecNested = go ""
  where
    go current (SpecNode name inhabited children) = "describe " . shows name . " (" . specs . ")"
      where
        specs :: ShowS
        specs = (sequenceS . addThis . map (go (current . showString name . "."))) children
        addThis
          | inhabited = ((current . showString name . "Spec.spec") :)
          | otherwise = id

data SpecNode = SpecNode String Bool [SpecNode]
  deriving (Eq, Show)

specNodeName :: SpecNode -> String
specNodeName (SpecNode name _ _) = name

specNodeInhabited :: SpecNode -> Bool
specNodeInhabited (SpecNode _ inhabited _) = inhabited

specNodeChildren :: SpecNode -> [SpecNode]
specNodeChildren (SpecNode _ _ children) = children

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  dirs <- filterM (doesDirectoryExist . (dir </>)) c
  files <- filterM (doesFileExist . (dir </>)) c
  return (dirs, files)

-- | Find specs relative to given source file.
--
-- The source file itself is not considered.
findSpecs :: FilePath -> IO [SpecNode]
findSpecs src = do
  let (dir, file) = splitFileName src
  (dirs, files) <- getFilesAndDirectories dir
  go dir (dirs, filter (/= file) files)
  where
    go :: FilePath -> ([FilePath], [FilePath]) -> IO [SpecNode]
    go base (dirs, files) = do
      nestedSpecs <- forM dirs $ \d -> do
        let dir = base </> d
        SpecNode d False <$> (getFilesAndDirectories dir >>= go dir)
      (return . filterSpecs . combineSpecs) (specsFromFiles files ++ nestedSpecs)
      where
        specsFromFiles = catMaybes . map specFromFile
          where
            suffixes :: [String]
            suffixes = ["Spec.hs","Spec.lhs"]

            specFromFile :: FilePath -> Maybe SpecNode
            specFromFile file = msum $ map (specFromFile_ file) suffixes

            specFromFile_ :: FilePath -> String -> Maybe SpecNode
            specFromFile_ file suffix
              | suffix `isSuffixOf` file = Just $ SpecNode (dropEnd (length suffix) file) True []
              | otherwise = Nothing

            dropEnd :: Int -> [a] -> [a]
            dropEnd n = reverse . drop n . reverse

        -- remove empty leafs
        filterSpecs :: [SpecNode] -> [SpecNode]
        filterSpecs = filter (\x -> specNodeInhabited x || (not . null . specNodeChildren) x)

        -- sort specs, and merge nodes with the same name
        combineSpecs :: [SpecNode] -> [SpecNode]
        combineSpecs = foldr f [] . sortBy (compare `on` specNodeName)
          where
            f x@(SpecNode n1 _ _) (y@(SpecNode n2 _ _):acc) | n1 == n2 = x `combine` y : acc
            f x acc = x : acc

            x `combine` y = SpecNode name inhabited children
              where
                name      = specNodeName x
                inhabited = specNodeInhabited x || specNodeInhabited y
                children  = specNodeChildren x ++ specNodeChildren y
