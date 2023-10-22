{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Formatters.V2Spec (spec) where

import           Prelude ()
import           Helper

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Spec as Spec
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Formatters.V2

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
formatConfig = defaultFormatConfig {
  formatConfigOutputUnicode = unicode
, formatConfigUseDiff = True
, formatConfigDiffContext = Just 3
, formatConfigExternalDiff = Nothing
, formatConfigPrettyPrint = True
, formatConfigPrettyPrintFunction = Just (H.configPrettyPrintFunction H.defaultConfig unicode)
} where
    unicode = True

runSpecWith :: Formatter -> H.Spec -> IO [String]
runSpecWith formatter = captureLines . H.hspecWithResult H.defaultConfig {H.configFormat = Just $ formatterToFormat formatter}

spec :: Spec
spec = do
  describe "indentChunks" $ do
    context "with Original" $ do
      it "does not indent single-line input" $ do
        indentChunks "  " [Original "foo"] `shouldBe` [PlainChunk "foo"]

      it "indents multi-line input" $ do
        indentChunks "  " [Original "foo\nbar\nbaz\n"] `shouldBe` [PlainChunk "foo\n  bar\n  baz\n  "]

    context "with Modified" $ do
      it "returns the empty list on empty input" $ do
        indentChunks "  " [Modified ""] `shouldBe` []

      it "does not indent single-line input" $ do
        indentChunks "  " [Modified "foo"] `shouldBe` [ColorChunk "foo"]

      it "indents multi-line input" $ do
        indentChunks "  " [Modified "foo\nbar\nbaz\n"] `shouldBe` [ColorChunk "foo", PlainChunk "\n  ", ColorChunk "bar", PlainChunk "\n  ", ColorChunk "baz", PlainChunk "\n", ColorChunk "  "]

      it "colorizes whitespace-only input" $ do
        indentChunks "  " [Modified "  "] `shouldBe` [ColorChunk "  "]

      it "colorizes whitespace-only lines" $ do
        indentChunks "  " [Modified "foo\n  \n"] `shouldBe` [ColorChunk "foo", PlainChunk "\n  ", ColorChunk "  ", PlainChunk "\n", ColorChunk "  "]

      it "colorizes whitespace at the end of the input" $ do
        indentChunks "  " [Modified "foo\n  "] `shouldBe` [ColorChunk "foo", PlainChunk "\n  ", ColorChunk "  "]

      it "splits off whitespace-only segments at the end of a line so that they get colorized" $ do
        indentChunks "  " [Modified "foo  \n"] `shouldBe` [ColorChunk "foo", ColorChunk "  ", PlainChunk "\n", ColorChunk "  "]

      it "splits off whitespace-only segments at the end of the input so that they get colorized" $ do
        indentChunks "  " [Modified "23 "] `shouldBe` [ColorChunk "23", ColorChunk " "]

      context "when next chunk starts with a newline" $ do
        it "splits off whitespace-only segments at the end of a chunk so that they get colorized" $ do
          indentChunks "  " [Modified "23 ", Original "\nbar"] `shouldBe` [ColorChunk "23", ColorChunk " ", PlainChunk "\n  bar"]

      context "when next chunk starts with spaces followed by a newline" $ do
        it "splits off whitespace-only segments at the end of a chunk so that they get colorized" $ do
          indentChunks "  " [Modified "23 ", Original "   \nbar"] `shouldBe` [ColorChunk "23", ColorChunk " ", PlainChunk "   \n  bar"]

      context "when all following chunks only consist of whitespace characters" $ do
        it "splits off whitespace-only segments at the end of a chunk so that they get colorized" $ do
          indentChunks "  " [Modified "23 ", Original "  ", Original "  "] `shouldBe` [ColorChunk "23", ColorChunk " ", PlainChunk "  ", PlainChunk "  "]

      context "when all following chunks only consist of whitespace characters until the next newline is encountered" $ do
        it "splits off whitespace-only segments at the end of a chunk so that they get colorized" $ do
          indentChunks "  " [Modified "23 ", Original " ", Modified " ", Original "\n"] `shouldBe` [ColorChunk "23", ColorChunk " ", PlainChunk " ", ColorChunk " ", PlainChunk "\n  "]

      context "when next chunk starts with a non-whitespace character" $ do
        it "does not split off whitespace-only segments at the end of a chunk" $ do
          indentChunks "  " [Modified "23 ", Original "bar"] `shouldBe` [ColorChunk "23 ", PlainChunk "bar"]

      context "when all following chunks only consist of non-newline characters until the next non-whitespace character is encountered" $ do
        it "does not split off whitespace-only segments at the end of a chunk" $ do
          indentChunks "  " [Modified "23 ", Original "  ", Original " bar"] `shouldBe` [ColorChunk "23 ", PlainChunk "  ", PlainChunk " bar"]

      context "with empty lines" $ do
        it "colorizes indentation" $ do
          indentChunks "  " [Original "foo", Modified "\n\n", Original "bar"] `shouldBe` [PlainChunk "foo", PlainChunk "\n", ColorChunk "  ", PlainChunk "\n", ColorChunk "  ", PlainChunk "bar"]

  describe "progress" $ do
    let item = ItemDone ([], "") . Item Nothing 0 ""
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

  describe "checks" $ do
    let
      formatter = checks
      config = H.defaultConfig { H.configFormat = Just $ formatterToFormat formatter }

    it "prints unicode check marks" $ do
      r <- captureLines . H.hspecWithResult config $ do
        H.it "foo" True
      normalizeSummary r `shouldBe` [
          ""
        , "foo [✔]"
        , ""
        , "Finished in 0.0000 seconds"
        , "1 example, 0 failures"
        ]

    it "uses ASCII as a fallback" $ do
      r <- captureLines . H.hspecWithResult config { H.configUnicodeMode = H.UnicodeNever } $ do
        H.it "foo" True
      normalizeSummary r `shouldBe` [
          ""
        , "foo [v]"
        , ""
        , "Finished in 0.0000 seconds"
        , "1 example, 0 failures"
        ]

  describe "specdoc" $ do

    let runSpec = runSpecWith specdoc

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
            pass
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
      let expectedButGot expected actual = ItemDone ([], "") . Item Nothing 0 "" $ Failure Nothing $ ExpectedButGot Nothing expected actual

      it "recovers unicode from ExpectedButGot" $ do
        formatter <- formatterToFormat failed_examples formatConfig { formatConfigOutputUnicode = True }
        formatter $ expectedButGot (show "\955") (show "\956")
        (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
            ""
          , "Failures:"
          , ""
          , "  1) "
          , "       expected: \"λ\""
          , "        but got: \"μ\""
          , ""
          , "  To rerun use: --match \"//\" --seed 0"
          , ""
          , "Randomized with seed 0"
          , ""
          , "Finished in 0.0000 seconds"
          , "1 example, 1 failure"
          ]

      context "on --expert" $ do
        it "does not print rerun message" $ do
          formatter <- formatterToFormat failed_examples formatConfig { formatConfigExpertMode = True }
          formatter $ expectedButGot "foo" "bar"
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Failures:"
            , ""
            , "  1) "
            , "       expected: foo"
            , "        but got: bar"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 1 failure"
            ]

      context "when actual/expected contain newlines" $ do
        it "adds indentation" $ do
          formatter <- formatterToFormat failed_examples formatConfig
          formatter $ expectedButGot "first\nsecond\nthird" "first\ntwo\nthird"
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
            , "  To rerun use: --match \"//\" --seed 0"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in 0.0000 seconds"
            , "1 example, 1 failure"
            ]

      context "without failures" $ do
        it "shows summary in green if there are no failures" $ do
          formatter <- formatterToFormat failed_examples formatConfig
          formatter . ItemDone ([], "") . Item Nothing 0 "" $ Success
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures"
            ]

      context "with pending examples" $ do
        it "shows summary in yellow if there are pending examples" $ do
          formatter <- formatterToFormat failed_examples formatConfig
          formatter . ItemDone ([], "") . Item Nothing 0 "" $ Pending Nothing Nothing
          (fmap normalizeSummary . captureLines) (formatter $ Done []) `shouldReturn` [
              ""
            , "Finished in 0.0000 seconds"
            , "1 example, 0 failures, 1 pending"
            ]

    context "same as failed_examples" $ do
      failed_examplesSpec specdoc

  describe "getExpectedTotalCount" $ do
    let formatter = silent { formatterStarted = fmap show getExpectedTotalCount >>= writeLine }
        runSpec = runSpecWith formatter
    it "returns the total number of spec items" $ do
      result:_ <- runSpec testSpec
      result `shouldBe` "6"

failed_examplesSpec :: Formatter -> Spec
failed_examplesSpec formatter = do
  let runSpec = runSpecWith formatter

  context "displays a detailed list of failures" $ do
    it "prints all requirements that are not met" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "  1) Example fail 1")

    it "prints the exception type for requirements that fail due to an uncaught exception" $ do
      r <- runSpec $ do
        H.it "foobar" (throw (ErrorCall "baz") :: Bool)
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
