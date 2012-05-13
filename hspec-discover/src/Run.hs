-- | A preprocessor that finds and combines specs.
module Run where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           System.Directory
import           System.FilePath

data SpecNode = SpecNode String Bool [SpecNode]
  deriving (Eq, Show)

findSpecs :: FilePath -> IO [SpecNode]
findSpecs dir = do
  c <- getDirectoryContents dir
  dirs <- filter (/= "..") . filter (/= ".") <$> filterM (doesDirectoryExist . (dir </>)) c
  nestedSpecs <- forM dirs $ \d -> do
    SpecNode d False <$> findSpecs (dir </> d)
  specs <- sort . filterSpecs <$> filterM (doesFileExist . (dir </>)) c
  return $ map (\x -> SpecNode x True []) specs ++ nestedSpecs
  where
    filterSpecs = map stripSuffix . filter (isSuffixOf suffix)
      where
        suffix = "Spec.hs"
        stripSuffix = reverse . drop (length suffix) . reverse
