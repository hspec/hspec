import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "read" $ do
    context "when used with ints" $ do
      prop "is inverse to show" $
        \x -> (read . show) x `shouldBe` (x :: Int)
