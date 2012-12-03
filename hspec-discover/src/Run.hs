{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A preprocessor that finds and combines specs.
module Run where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.String
import           Data.Function
import           System.Environment
import           System.Exit
import           System.IO
import           System.Directory
import           System.FilePath hiding (combine)

instance IsString ShowS where
  fromString = showString

run :: [String] -> IO ()
run args_ = case args_ of
  src : _ : dst : args -> do
    nested <- case args of
      []           -> return False
      ["--nested"] -> return True
      _            -> exit
    specs <- findSpecs src
    writeFile dst (mkSpecModule src nested specs)
  _ -> exit
  where
    exit = do
      name <- getProgName
      hPutStrLn stderr ("usage: " ++ name ++ " SRC CUR DST [--nested]")
      exitFailure

mkSpecModule :: FilePath -> Bool -> [SpecNode] -> String
mkSpecModule src nested nodes =
  ( "{-# LINE 1 " . shows src . " #-}"
  . showString "module Main where\n"
  . showString "import Test.Hspec.Meta\n"
  . importList nodes
  . showString "main :: IO ()\n"
  . showString "main = hspec $ "
  . format nodes
  ) "\n"
  where
    format
      | nested    = formatSpecsNested
      | otherwise = formatSpecs

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
        specsFromFiles = map (\x -> SpecNode (stripSuffix x) True []) . filter (isSuffixOf suffix)
          where
            suffix = "Spec.hs"
            stripSuffix = reverse . drop (length suffix) . reverse

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
