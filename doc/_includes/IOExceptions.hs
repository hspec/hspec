import Test.Hspec
import System.IO.Error (isDoesNotExistError)

main :: IO ()
main = hspec $ do
  describe "readFile" $ do
    context "when specified file does not exist" $ do
      it "throws an exception" $ do
        readFile "none-existing-file" `shouldThrow` isDoesNotExistError
