-- | A preprocessor that finds and combines specs.
module Run where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           System.Directory
import           System.FilePath

data SpecTree = SpecNode {
  specNodeName      :: String
, specNodeInhabited :: Bool
, specNodeChildren  :: [SpecTree]
} deriving (Eq, Show)

findSpecs :: FilePath -> IO [SpecTree]
findSpecs dir = do
  c <- getDirectoryContents dir
  specs <- filterSpecs <$> filterM (doesFileExist . (dir </>)) c
  return $ map (\x -> SpecNode x True []) specs
  where
    filterSpecs = map stripSuffix . filter (isSuffixOf suffix)
      where
        suffix = "Spec.hs"
        stripSuffix = reverse . drop (length suffix) . reverse
