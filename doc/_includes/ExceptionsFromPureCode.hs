import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "div" $ do
    it "throws an exception if the second argument is 0" $ do
      evaluate (1 `div` 0 :: Int) `shouldThrow` anyArithException

  describe "evaluate" $ do
    it "forces exceptions" $ do
      evaluate ('a' : undefined) `shouldThrow` anyErrorCall

  describe "$!!" $ do
    it "forces exceptions" $ do
      mapM evaluate ('a' : undefined) `shouldThrow` anyErrorCall
