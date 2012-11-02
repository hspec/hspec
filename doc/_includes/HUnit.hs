import Test.Hspec
import Test.HUnit

main :: IO ()
main = hspec $ do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1, 2, 3] @?= [3, 2, 1 :: Int]
