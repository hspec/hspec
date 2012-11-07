import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` errorCall "Prelude.head: empty list"
