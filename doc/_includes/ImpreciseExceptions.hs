import Test.Hspec
import Control.Applicative ((<$>), (<*>))
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "an exceptional value" $ do
    it "contains a set of exceptions (semantically)" $ do
      evaluate (error "foo" + error "bar" :: Int)
        `shouldThrow` (||) <$> errorCall "foo" <*> errorCall "bar"
