module Test.Hspec.Core.TypeSpec (main, spec) where

import           Test.Hspec.Meta
import           Test.QuickCheck

import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Control.Exception (AsyncException(..), throwIO)
import qualified Test.Hspec.Core.Type as H
import qualified Test.Hspec.Pending as H (pending)

main :: IO ()
main = hspec spec

evaluateExample :: H.Example e => e -> IO H.Result
evaluateExample = H.evaluateExample H.defaultParams

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "for Bool" $ do
      it "returns Success on True" $ do
        evaluateExample True `shouldReturn` H.Success

      it "returns Fail on False" $ do
        evaluateExample False `shouldReturn` H.Fail ""

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Bool) `shouldThrow` errorCall "foobar"

    context "for Expectation" $ do
      it "returns Success if all expectations hold" $ do
        evaluateExample (23 `shouldBe` (23 :: Int)) `shouldReturn` H.Success

      it "returns Fail if an expectation does not hold" $ do
        evaluateExample (23 `shouldBe` (42 :: Int)) `shouldReturn` H.Fail "expected: 42\n but got: 23"

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Expectation) `shouldThrow` errorCall "foobar"

    context "for Property" $ do
      it "returns Success if property holds" $ do
        evaluateExample (property $ \n -> n == (n :: Int)) `shouldReturn` H.Success

      it "returns Fail if property does not hold" $ do
        H.Fail _ <- evaluateExample $ property $ \n -> n /= (n :: Int)
        return ()

      it "shows what falsified it" $ do
        H.Fail r <- evaluateExample $ property $ \ n -> n == (n + 1 :: Int)
        lines r `shouldSatisfy` any (== "0")

      it "propagates UserInterrupt" $ do
        let p = morallyDubiousIOProperty (throwIO UserInterrupt >> return True)
        evaluateExample p `shouldThrow` (== UserInterrupt)

      it "propagates exceptions" $ do
        pending "this probaly needs a patch to QuickCheck"
        -- evaluateExample (property $ (error "foobar" :: Int -> Bool)) `shouldThrow` errorCall "foobar"

    context "for pending" $ do
      it "returns Pending" $ do
        evaluateExample (H.pending) `shouldReturn` H.Pending Nothing

      it "includes the optional reason" $ do
        evaluateExample (H.pending "foo") `shouldReturn` H.Pending (Just "foo")
