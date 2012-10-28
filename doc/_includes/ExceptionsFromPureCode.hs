import Test.Hspec
import Control.Exception (evaluate)
import Control.DeepSeq (($!!))

main :: IO ()
main = hspec $ do
  describe "div" $ do
    it "throws an exception if the second argument is 0" $ do
      evaluate (1 `div` 0 :: Int) `shouldThrow` anyArithException

  describe "evaluate" $ do
    it "forces exceptions" $ do
      evaluate ('a' : undefined) `shouldThrow` errorCall "Prelude.undefined"

  describe "$!!" $ do
    it "forces exceptions" $ do
      (return $!! 'a' : undefined) `shouldThrow` errorCall "Prelude.undefined"
