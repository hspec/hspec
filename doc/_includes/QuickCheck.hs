import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
