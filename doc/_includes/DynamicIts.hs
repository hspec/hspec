import Test.Hspec
import Control.Monad
import System.IO
import System.Directory

main :: IO ()
main = hspec $ do
  describe "generate specs" $ do
    allFiles <- runIO (getDirectoryContents ".")
    let files = filter (`notElem` [".", ".."]) allFiles
    forM_ files $ \file -> do
      it ("test using the file " ++ file) $ do
        pending

