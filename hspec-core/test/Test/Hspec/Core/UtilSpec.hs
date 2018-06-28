module Test.Hspec.Core.UtilSpec (spec) where

import           Helper
import           Control.Concurrent
import qualified Control.Exception as E

import           Test.Hspec.Core.Util

spec :: Spec
spec = do
  describe "pluralize" $ do
    it "returns singular when used with 1" $ do
      pluralize 1 "thing" `shouldBe` "1 thing"

    it "returns plural when used with number greater 1" $ do
      pluralize 2 "thing" `shouldBe` "2 things"

    it "returns plural when used with 0" $ do
      pluralize 0 "thing" `shouldBe` "0 things"

  describe "formatException" $ do
    it "converts exception to string" $ do
      formatException (E.toException E.DivideByZero) `shouldBe` "ArithException\ndivide by zero"

    context "when used with an IOException" $ do
      it "includes the IOErrorType" $ do
        inTempDirectory $ do
          Left e <- E.try (readFile "foo")
          formatException e `shouldBe` intercalate "\n" [
              "IOException of type NoSuchThing"
            , "foo: openFile: does not exist (No such file or directory)"
            ]

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
      Left e <- safeTry throwException
      show e `shouldBe` "foobar"

    it "evaluates result to weak head normal form" $ do
      Left e <- safeTry (return $ E.throw $ E.ErrorCall "foo")
      show e `shouldBe` "foo"

    it "does not catch asynchronous exceptions" $ do
      mvar <- newEmptyMVar
      sync <- newEmptyMVar
      threadId <- forkIO $ do
        safeTry (putMVar sync () >> threadDelay 1000000) >> return ()
        `E.catch` putMVar mvar
      takeMVar sync
      throwTo threadId E.UserInterrupt
      readMVar mvar `shouldReturn` E.UserInterrupt

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

    it "succeeds with a pattern that matches the message given in the failure list" $ do
      let p = filterPredicate "ModuleA.ModuleB.foo does something"
      p (["ModuleA", "ModuleB", "foo"], "does something") `shouldBe` True

    context "with an absolute path that begins or ends with a slash" $ do
      it "succeeds" $ do
        let p = filterPredicate "/foo/bar/baz/example 1/"
        p (["foo", "bar", "baz"], "example 1") `shouldBe` True

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
