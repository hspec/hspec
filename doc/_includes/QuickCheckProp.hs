import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "read" $ do
    context "when used with Int" $ do
      prop "is inverse to show" $
        \x -> (read . show) x `shouldBe` (x :: Int)
