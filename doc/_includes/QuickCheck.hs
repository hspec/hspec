import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "read" $ do
    context "when used with Int" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x `shouldBe` (x :: Int)
