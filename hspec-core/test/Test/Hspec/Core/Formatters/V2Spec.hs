{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Formatters.V2Spec (spec) where

import           Prelude ()
import           Helper
import qualified Control.Exception as E

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Spec as Spec
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Formatters.V2
import qualified Test.Hspec.Core.Formatters.Monad as H

testSpec :: H.Spec
testSpec = do
  H.describe "Example" $ do
    H.it "success"    (H.Result "" Spec.Success)
    H.it "fail 1"     (H.Result "" $ Spec.Failure Nothing $ H.Reason "fail message")
    H.it "pending"    (H.pendingWith "pending message")
    H.it "fail 2"     (H.Result "" $ Spec.Failure Nothing H.NoReason)
    H.it "exceptions" (undefined :: Spec.Result)
    H.it "fail 3"     (H.Result "" $ Spec.Failure Nothing H.NoReason)


formatConfig :: FormatConfig
formatConfig = FormatConfig {
  formatConfigUseColor = False
, formatConfigUseDiff = True
, formatConfigPrintTimes = False
, formatConfigHtmlOutput = False
, formatConfigPrintCpuTime = False
, formatConfigUsedSeed = 0
, formatConfigItemCount = 0
}

spec :: Spec
spec = do
  let item = ItemDone ([], "") . Item Nothing 0 ""

  describe "progress" $ do
    describe "formatterItemDone" $ do
      it "marks succeeding examples with ." $ do
        formatter <- formatterToFormat progress formatConfig
        captureLines (formatter $ item Success)
          `shouldReturn` ["."]

      it "marks failing examples with F" $ do
        formatter <- formatterToFormat progress formatConfig
        captureLines (formatter . item $ Failure Nothing NoReason)
          `shouldReturn` ["F"]

      it "marks pending examples with ." $ do
        formatter <- formatterToFormat progress formatConfig
        captureLines (formatter . item $ Pending Nothing Nothing)
          `shouldReturn` ["."]

  describe "specdoc" $ do
    let
      runSpec = captureLines . H.hspecWithResult H.defaultConfig {H.configFormat = Just $ formatterToFormat specdoc}

    it "displays a header for each thing being described" $ do
      _:x:_ <- runSpec testSpec
      x `shouldBe` "Example"

    it "displays one row for each behavior" $ do
      r <- runSpec $ do
        H.describe "List as a Monoid" $ do
          H.describe "mappend" $ do
            H.it "is associative" True
          H.describe "mempty" $ do
            H.it "is a left identity" True
            H.it "is a right identity" True
        H.describe "Maybe as a Monoid" $ do
          H.describe "mappend" $ do
            H.it "is associative" True
          H.describe "mempty" $ do
            H.it "is a left identity" True
            H.it "is a right identity" True
      normalizeSummary r `shouldBe` [
          ""
        , "List as a Monoid"
        , "  mappend"
        , "    is associative"
        , "  mempty"
        , "    is a left identity"
        , "    is a right identity"
        , "Maybe as a Monoid"
        , "  mappend"
        , "    is associative"
        , "  mempty"
        , "    is a left identity"
        , "    is a right identity"
        , ""
        , "Finished in 0.0000 seconds"
        , "6 examples, 0 failures"
        ]

    it "outputs an empty line at the beginning (even for non-nested specs)" $ do
      r <- runSpec $ do
        H.it "example 1" True
        H.it "example 2" True
      normalizeSummary r `shouldBe` [
          ""
        , "example 1"
        , "example 2"
        , ""
        , "Finished in 0.0000 seconds"
        , "2 examples, 0 failures"
        ]

    it "displays a row for each successful, failed, or pending example" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "  fail 1 FAILED [1]")
      r `shouldSatisfy` any (== "  success")

    it "displays a '#' with an additional message for pending examples" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "    # PENDING: pending message")

    context "with an empty group" $ do
      it "omits that group from the report" $ do
        r <- runSpec $ do
          H.describe "foo" $ do
            H.it "example 1" True
          H.describe "bar" $ do
            return ()
          H.describe "baz" $ do
            H.it "example 2" True

        normalizeSummary r `shouldBe` [
            ""
          , "foo"
          , "  example 1"
          , "baz"
          , "  example 2"
          , ""
          , "Finished in 0.0000 seconds"
          , "2 examples, 0 failures"
          ]

    describe "formatterDone" $ do
      context "when actual/expected contain newlines" $ do
        it "adds indentation" $ do
          formatter <- formatterToFormat progress formatConfig
          _ <- formatter .  ItemDone ([], "") . Item Nothing 0 "" $ Failure Nothing $ ExpectedButGot Nothing "first\nsecond\nthird" "first\ntwo\nthird"
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Failures:"
            , ""
            , "  1) "
            , "       expected: first"
            , "                 second"
            , "                 third"
            , "        but got: first"
            , "                 two"
            , "                 third"
            , ""
            , "  To rerun use: --match \"//\""
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 1 failure"
            ]

      context "without failures" $ do
        it "shows summary in green if there are no failures" $ do
          formatter <- formatterToFormat progress formatConfig
          _ <- formatter .  ItemDone ([], "") . Item Nothing 0 "" $ Success
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures"
            ]

      context "with pending examples" $ do
        it "shows summary in yellow if there are pending examples" $ do
          formatter <- formatterToFormat progress formatConfig
          _ <- formatter .  ItemDone ([], "") . Item Nothing 0 "" $ Pending Nothing Nothing
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures, 1 pending"
            ]

    context "same as failed_examples" $ do
      failed_examplesSpec specdoc

  describe "additional formatter features" $ do
    describe "getFinalCount" $ do
      let formatter = silent {H.formatterDone = fmap show H.getFinalCount >>= H.writeLine}
          runSpec = captureLines . H.hspecWithResult H.defaultConfig {H.configFormat = Just $ formatterToFormat formatter}
      it "counts examples" $ do
        result:_ <- runSpec testSpec
        result `shouldBe` "6"

failed_examplesSpec :: H.Formatter -> Spec
failed_examplesSpec formatter = do
  let runSpec = captureLines . H.hspecWithResult H.defaultConfig {H.configFormat = Just $ formatterToFormat formatter}

  context "displays a detailed list of failures" $ do
    it "prints all requirements that are not met" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "  1) Example fail 1")

    it "prints the exception type for requirements that fail due to an uncaught exception" $ do
      r <- runSpec $ do
        H.it "foobar" (E.throw (E.ErrorCall "baz") :: Bool)
      r `shouldContain` [
          "  1) foobar"
        , "       uncaught exception: ErrorCall"
        , "       baz"
        ]

    it "prints all descriptions when a nested requirement fails" $ do
      r <- runSpec $
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" False
      r `shouldSatisfy` any (== "  1) foo.bar baz")


    context "when a failed example has a source location" $ do
      it "includes that source location above the error message" $ do
        let loc = H.Location "test/FooSpec.hs" 23 4
            addLoc e = e {Spec.itemLocation = Just loc}
        r <- runSpec $ H.mapSpecItem_ addLoc $ do
          H.it "foo" False
        r `shouldContain` ["  test/FooSpec.hs:23:4: ", "  1) foo"]
