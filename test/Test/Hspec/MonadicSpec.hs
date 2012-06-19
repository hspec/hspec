module Test.Hspec.MonadicSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           System.IO (stdout)
import           System.IO.Silently

import qualified Test.Hspec.Monadic as H

main :: IO ()
main = hspec spec

run :: H.Spec -> IO [String]
-- FIXME: use a mocked file handle for `stdout` instead of `capture`
run = fmap (lines . fst) . capture . H.hHspec stdout

spec :: Spec
spec = do

  describe "A failing example" $ do
    it "is reported" $ do
      r <- run $ do
        H.describe "Foo" $ do
          H.it "does something" False
      r `shouldSatisfy` any (== "1) Foo does something FAILED")

    context "when nested" $ do
      it "is reported" $ do
        r <- run $ do
          H.describe "Foo" $ H.describe "Bar" $ H.describe "baz" $ H.context "when condition" $ do
            H.it "does something" False
        r `shouldSatisfy` any (== "1) Foo - Bar - baz - when condition - does something FAILED")
