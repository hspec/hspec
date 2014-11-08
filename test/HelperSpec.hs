module HelperSpec (main, spec) where

import           Helper
import           System.IO.Error (isDoesNotExistError)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "withFileContent" $ do
    it "creates a file with specified content and runs specified action" $ do
      withFileContent "foo" $ \file -> do
        readFile file `shouldReturn` "foo"

    it "removes file after action has been run" $ do
      file <- withFileContent "foo" return
      readFile file `shouldThrow` isDoesNotExistError
