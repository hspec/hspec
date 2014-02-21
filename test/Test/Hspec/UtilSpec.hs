module Test.Hspec.UtilSpec (main, spec) where

import           Helper
import qualified Control.Exception as E

import           Test.Hspec.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pluralize" $ do
    it "returns an amount and a word given an amount and word" $ do
      pluralize 1 "thing" `shouldBe` "1 thing"

    it "returns a singular word given the number 1" $ do
      pluralize 1 "thing" `shouldBe` "1 thing"

    it "returns a plural word given a number greater than 1" $ do
      pluralize 2 "thing" `shouldBe` "2 things"

    it "returns a plural word given the number 0" $ do
      pluralize 0 "thing" `shouldBe` "0 things"

  describe "lineBreaksAt" $ do
    it "inserts line breaks at word boundaries" $ do
      lineBreaksAt 20 "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod"
      `shouldBe` [
          "Lorem ipsum dolor"
        , "sit amet,"
        , "consectetur"
        , "adipisicing elit,"
        , "sed do eiusmod"
        ]

  describe "safeTry" $ do
    it "returns Right on success" $ do
      Right e <- safeTry (return 23 :: IO Int)
      e `shouldBe` 23

    it "returns Left on exception" $ do
      Left e <- safeTry (E.throwIO E.DivideByZero :: IO Int)
      show e `shouldBe` "divide by zero"

    it "evaluates result to weak head normal form" $ do
      Left e <- safeTry (return undefined)
      show e `shouldBe` "Prelude.undefined"

    it "re-throws AsyncException" $ do
      safeTry (E.throwIO E.UserInterrupt :: IO Int) `shouldThrow` (== E.UserInterrupt)

  describe "filterPredicate" $ do
    it "tries to match a pattern against a path" $ do
      let p = filterPredicate "foo/bar/example 1"
      p (["foo", "bar"], "example 1") `shouldBe` True
      p (["foo", "bar"], "example 2") `shouldBe` False

    it "is ambiguous" $ do
      let p = filterPredicate "foo/bar/baz"
      p (["foo", "bar"], "baz") `shouldBe` True
      p (["foo"], "bar/baz") `shouldBe` True

    it "succeeds on a partial match" $ do
      let p = filterPredicate "bar/baz"
      p (["foo", "bar", "baz"], "example 1") `shouldBe` True

    it "succeeds with a pattern that matches the message give in the failure list" $ do
      let p = filterPredicate "ModuleA.ModuleB.foo does something"
      p (["ModuleA", "ModuleB", "foo"], "does something") `shouldBe` True

  describe "formatRequirement" $ do
    it "creates a sentence from a subject and a requirement" $ do
      formatRequirement (["reverse"], "reverses a list") `shouldBe` "reverse reverses a list"

    it "creates a sentence from a subject and a requirement when the subject consits of multiple words" $ do
      formatRequirement (["The reverse function"], "reverses a list") `shouldBe` "The reverse function reverses a list"

    it "returns the requirement if no subject is given" $ do
      formatRequirement ([], "reverses a list") `shouldBe` "reverses a list"

    it "inserts context separated by commas" $ do
      formatRequirement (["reverse", "when applied twice"], "reverses a list") `shouldBe` "reverse, when applied twice, reverses a list"

    it "joins components of a subject with a dot" $ do
      formatRequirement (["Data", "List", "reverse"], "reverses a list") `shouldBe` "Data.List.reverse reverses a list"

    it "properly handles context after a subject that consists of several components" $ do
      formatRequirement (["Data", "List", "reverse", "when applied twice"], "reverses a list") `shouldBe` "Data.List.reverse, when applied twice, reverses a list"
