-- Spec.hs
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prelude" $ do
    describe "reverse" $ do
      it "reverses a list" False
    describe "show" $ do
      it "shows its argument" True
