{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Core.Runner.EvalSpec (spec) where

import           Prelude ()
import           Helper

import           NonEmpty (fromList)

import           Test.Hspec.Core.Spec (FailureReason(..), Result(..), ResultStatus(..), Location(..))

import           Test.Hspec.Core.Runner.Eval

instance Arbitrary ResultStatus where
  arbitrary = oneof [
      pure Success
    , Pending <$> arbitrary <*> arbitrary
    , failure
    ]

instance Arbitrary FailureReason where
  arbitrary = oneof [
      pure NoReason
    , ExpectedButGot <$> arbitrary <*> (show <$> positive) <*> (show <$> positive)
    , Error <$> arbitrary <*> pure (toException DivideByZero)
    ]

instance Arbitrary Location where
  arbitrary = Location <$> elements ["src/Foo.hs", "src/Bar.hs", "src/Baz.hs"] <*> positive <*> positive

positive :: Gen Int
positive = getPositive <$> arbitrary

failureResult :: Gen Result
failureResult = Result <$> arbitrary <*> failure

pendingResult :: Gen Result
pendingResult = Result <$> arbitrary <*> pending

successResult :: Gen Result
successResult = Result <$> arbitrary <*> pure Success

pending :: Gen ResultStatus
pending = Pending <$> arbitrary <*> arbitrary

failure :: Gen ResultStatus
failure = Failure <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "mergeResults" $ do
    it "gives failures from items precedence" $ do
      forAll failureResult $ \ item hook -> do
        mergeResults Nothing item hook `shouldBe` item

    it "gives failures from hooks precedence over succeeding items" $ do
      forAll successResult $ \ item@(Result info _) -> forAll failure $ \ hook -> do
        mergeResults Nothing item hook `shouldBe` Result info hook

    it "gives failures from hooks precedence over pending items" $ do
      forAll pendingResult $ \ item@(Result info _) -> forAll failure $ \ hook -> do
        mergeResults Nothing item hook `shouldBe` Result info hook

    it "gives pending items precedence over pending hooks" $ do
      forAll pendingResult $ \ item -> forAll pending $ \ hook -> do
        mergeResults Nothing item hook `shouldBe` item

  describe "traverse" $ do
    context "when used with Tree" $ do
      let
        tree :: Tree () Int
        tree = Node "" $ fromList [Node "" $ fromList [Leaf 1, Node "" $ fromList [Leaf 2, Leaf 3]], Leaf 4]
      it "walks the tree left-to-right, depth-first" $ do
        ref <- newIORef []
        traverse_ (modifyIORef ref . (:) ) tree
        reverse <$> readIORef ref `shouldReturn` [1 .. 4]
