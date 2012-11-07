import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "div" $ do
    it "throws an exception if the second argument is 0" $ do
      evaluate (1 `div` 0 :: Int) `shouldThrow` anyArithException

  describe "evaluate" $ do
    it "forces undefined list items" $ do
      evaluate [undefined] `shouldThrow` anyErrorCall

  describe "mapM evaluate" $ do
    it "forces undefined list items" $ do
      mapM evaluate [undefined] `shouldThrow` anyErrorCall
