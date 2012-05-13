-- | A preprocessor that finds and combines specs.
module Run where
import           Control.Monad
import           Control.Applicative
import           Data.List
import           Data.Function
import           System.Directory
import           System.FilePath hiding (combine)

data SpecNode = SpecNode String Bool [SpecNode]
  deriving (Eq, Show)

specNodeName :: SpecNode -> String
specNodeName (SpecNode name _ _) = name

specNodeInhabited :: SpecNode -> Bool
specNodeInhabited (SpecNode _ inhabited _) = inhabited

specNodeChildren :: SpecNode -> [SpecNode]
specNodeChildren (SpecNode _ _ children) = children

findSpecs :: FilePath -> IO [SpecNode]
findSpecs dir = do
  c <- getDirectoryContents dir
  dirs <- filter (/= "..") . filter (/= ".") <$> filterM (doesDirectoryExist . (dir </>)) c
  nestedSpecs <- forM dirs $ \d -> do
    SpecNode d False <$> findSpecs (dir </> d)
  files <- filterM (doesFileExist . (dir </>)) c
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
