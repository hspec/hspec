import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "head" $ do
    context "when applied to an empty list" $ do
      it "throws an exception" $ do
        head [] `shouldThrow` errorCall "Prelude.head: empty list"
