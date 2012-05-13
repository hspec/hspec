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
run args = case args of
  [src, _, dst] -> do
    specs <- findSpecs src
    writeFile dst (mkSpecModule specs)
  _ -> do
    name <- getProgName
    hPutStrLn stderr ("usage: " ++ name ++ " SRC CUR DST")
    exitFailure

mkSpecModule :: [SpecNode] -> String
mkSpecModule specs =
  ( showString "module Main where\n"
  . showString "import Test.Hspec.Monadic\n"
  . importList specs
  . showString "main :: IO ()\n"
  . showString "main = hspecX $ "
  . formatSpecs specs
  ) "\n"

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
formatSpecs = sequenceS . map formatSpec

-- | Convert a spec to code.
formatSpec :: SpecNode -> ShowS
formatSpec = go ""
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
      return $ combineSpecs (specsFromFiles files ++ nestedSpecs)
      where
        specsFromFiles = map (\x -> SpecNode (stripSuffix x) True []) . filter (isSuffixOf suffix)
          where
            suffix = "Spec.hs"
            stripSuffix = reverse . drop (length suffix) . reverse

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
