{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Hspec.ExpectationsSpec (spec) where

import           Control.Exception
import           Test.HUnit.Lang
import           Test.Hspec (Spec, describe, it)

import           Test.Hspec.Expectations hiding (HasCallStack)
import           Data.CallStack

expectationFailed :: HasCallStack => FailureReason -> HUnitFailure -> Bool
expectationFailed msg (HUnitFailure l m) = m == msg && (fmap setColumn l) == (fmap setColumn location)
  where
    location = case reverse callStack of
      [] -> Nothing
      (_, loc) : _ -> Just loc
    location :: Maybe SrcLoc

    setColumn loc_ = loc_{srcLocStartCol = 0, srcLocEndCol = 0}

spec :: Spec
spec = do
  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      "foo" `shouldBe` "foo"

    it "fails if arguments are not equal" $ do
      ("foo" `shouldBe` "bar") `shouldThrow` expectationFailed (ExpectedButGot Nothing "\"bar\"" "\"foo\"")

  describe "shouldSatisfy" $ do
    it "succeeds if value satisfies predicate" $ do
      "" `shouldSatisfy` null

    it "fails if value does not satisfy predicate" $ do
      ("foo" `shouldSatisfy` null) `shouldThrow` expectationFailed (Reason "predicate failed on: \"foo\"")

  describe "shouldReturn" $ do
    it "succeeds if arguments represent equal values" $ do
      return "foo" `shouldReturn` "foo"

    it "fails if arguments do not represent equal values" $ do
      (return "foo" `shouldReturn` "bar") `shouldThrow` expectationFailed (ExpectedButGot Nothing "\"bar\"" "\"foo\"")

  describe "shouldStartWith" $ do
    it "succeeds if second is prefix of first" $ do
      "hello world" `shouldStartWith` "hello"

    it "fails if second is not prefix of first" $ do
      ("hello world" `shouldStartWith` "world") `shouldThrow` expectationFailed (Reason "\"hello world\" does not start with \"world\"")

  describe "shouldEndWith" $ do
    it "succeeds if second is suffix of first" $ do
      "hello world" `shouldEndWith` "world"

    it "fails if second is not suffix of first" $ do
      ("hello world" `shouldEndWith` "hello") `shouldThrow` expectationFailed (Reason "\"hello world\" does not end with \"hello\"")

  describe "shouldContain" $ do
    it "succeeds if second argument is contained in the first" $ do
      "I'm an hello world message" `shouldContain` "an hello"

    it "fails if first argument does not contain the second" $ do
      ("foo" `shouldContain` "bar") `shouldThrow` expectationFailed (Reason "\"foo\" does not contain \"bar\"")

  describe "shouldNotBe" $ do
    it "succeeds if arguments are not equal" $ do
      "foo" `shouldNotBe` "bar"

    it "fails if arguments are equal" $ do
      ("foo" `shouldNotBe` "foo") `shouldThrow` expectationFailed (Reason "not expected: \"foo\"")

  describe "shouldNotSatisfy" $ do
    it "succeeds if value does not satisfy predicate" $ do
      "bar" `shouldNotSatisfy` null

    it "fails if the value does satisfy predicate" $ do
      ("" `shouldNotSatisfy` null) `shouldThrow` expectationFailed (Reason "predicate succeeded on: \"\"")

  describe "shouldNotReturn" $ do
    it "succeeds if arguments does not represent equal values" $ do
      return "foo" `shouldNotReturn` "bar"

    it "fails if arguments do represent equal values" $ do
      (return "foo" `shouldNotReturn` "foo") `shouldThrow` expectationFailed (Reason "not expected: \"foo\"")

  describe "shouldNotContain" $ do
    it "succeeds if second argument is not contained in the first" $ do
      "I'm an hello world message" `shouldNotContain` "test"

    it "fails if first argument does contain the second" $ do
      ("foo abc def" `shouldNotContain` "def") `shouldThrow` expectationFailed (Reason "\"foo abc def\" does contain \"def\"")

  describe "shouldThrow" $ do
    it "can be used to require a specific exception" $ do
      throwIO DivideByZero `shouldThrow` (== DivideByZero)

    it "can be used to require any exception" $ do
      error "foobar" `shouldThrow` anyException

    it "can be used to require an exception of a specific type" $ do
      error "foobar" `shouldThrow` anyErrorCall

    it "can be used to require a specific exception" $ do
      error "foobar" `shouldThrow` errorCall "foobar"

    it "fails, if a required specific exception is not thrown" $ do
      (throwIO Overflow `shouldThrow` (== DivideByZero)) `shouldThrow` expectationFailed (Reason "predicate failed on expected exception: ArithException (arithmetic overflow)")

    it "fails, if any exception is required, but no exception is thrown" $ do
      (return () `shouldThrow` anyException) `shouldThrow` expectationFailed (Reason "did not get expected exception: SomeException")

    it "fails, if an exception of a specific type is required, but no exception is thrown" $ do
      (return () `shouldThrow` anyErrorCall) `shouldThrow` expectationFailed (Reason "did not get expected exception: ErrorCall")
