{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Core.FormattersSpec (spec) where

import           Prelude ()
import           Helper
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import qualified Control.Exception as E

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Formatters as H
import qualified Test.Hspec.Core.Formatters.Monad as H
import           Test.Hspec.Core.Formatters.Monad hiding (interpretWith)

interpret :: FormatM a -> IO TextBlocks
interpret = interpretWith environment

simplify :: TextBlocks -> TextBlocks
simplify = id

removeColors :: TextBlocks -> String
removeColors = execWriter . interpretText env . renderTextBlocks
  where
    env = TextEnvironment{..}
    envRenderTextSpan = tell . tsText
    envMoveCursorUp _ = pure ()
    envClearFromCursorToEnd = pure ()
    envClearLine = pure ()

interpretWith :: Environment (StateT TextBlocks IO) -> FormatM a -> IO TextBlocks
interpretWith env = fmap simplify . flip execStateT mempty . H.interpretWith env

environment :: Environment (StateT TextBlocks IO)
environment = Environment {
  environmentGetSuccessCount = return 0
, environmentGetPendingCount = return 0
, environmentGetFailMessages = return []
, environmentUsedSeed = return 0
, environmentGetCPUTime = return Nothing
, environmentGetRealTime = return 0
, environmentWrite = \tb -> do
    blocks <- get
    modify $ fst . appendTextBlock tb
    return (textBlocksCount blocks)
, environmentInsert = \_ _ -> error "insert is not implemented here"
, environmentRewrite = \i f -> modify (modifyTextBlock i f)
, environmentUseDiff = return True
, environmentLiftIO = liftIO
}

testSpec :: H.Spec
testSpec = do
  H.describe "Example" $ do
    H.it "success"    (H.Result "" H.Success)
    H.it "fail 1"     (H.Result "" $ H.Failure Nothing $ H.Reason "fail message")
    H.it "pending"    (H.pendingWith "pending message")
    H.it "fail 2"     (H.Result "" $ H.Failure Nothing H.NoReason)
    H.it "exceptions" (undefined :: H.Result)
    H.it "fail 3"     (H.Result "" $ H.Failure Nothing H.NoReason)

spec :: Spec
spec = do
  describe "progress" $ do
    let mkFormatter = H.progress

    describe "exampleSucceeded" $ do
      it "marks succeeding examples with ." $ do
        formatter <- mkFormatter
        interpret (H.exampleSucceeded formatter undefined undefined) `shouldReturn` textBlocks [
            withSuccessColor "."
          ]

    describe "exampleFailed" $ do
      it "marks failing examples with F" $ do
        formatter <- mkFormatter
        interpret (H.exampleFailed formatter undefined undefined undefined) `shouldReturn` textBlocks [
            withFailColor "F"
          ]

    describe "examplePending" $ do
      it "marks pending examples with ." $ do
        formatter <- mkFormatter
        interpret (H.examplePending formatter undefined undefined undefined) `shouldReturn` textBlocks [
            withPendingColor "."
          ]
  describe "specdoc" $ specdoc H.defaultConfig {H.configFormatter = Just H.specdoc}
  describe "specdoc (with -j)" $ specdoc H.defaultConfig {H.configFormatter = Just H.specdoc, H.configConcurrentJobs = Just 4 }

specdoc :: H.Config -> Spec
specdoc config = do
    let
      Just formatterIO = H.configFormatter config
      runSpec = captureLines . H.hspecWithResult config

    it "displays a header for each thing being described" $ do
      _:x:_ <- runSpec testSpec
      x `shouldBe` "Example"

    it "displays one row for each behavior" $ do
      r <- runSpec $ do
        H.describe "Properties" $ do
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
        , "Properties"
        , "  List as a Monoid"
        , "    mappend"
        , "      is associative"
        , "    mempty"
        , "      is a left identity"
        , "      is a right identity"
        , "  Maybe as a Monoid"
        , "    mappend"
        , "      is associative"
        , "    mempty"
        , "      is a left identity"
        , "      is a right identity"
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

    it "displays a row for each successfull, failed, or pending example" $ do
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

    describe "failedFormatter" $ do
      context "when actual/expected contain newlines" $ do
        let
          env = environment {
            environmentGetFailMessages = return [FailureRecord Nothing ([], "") (ExpectedButGot Nothing "first\nsecond\nthird" "first\ntwo\nthird")]
            }
        it "adds indentation" $ do
          formatter <- formatterIO
          let action = H.failedFormatter formatter
          fmap removeColors (interpretWith env action) `shouldReturn` unlines [
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
#if __GLASGOW_HASKELL__ == 800
            , "WARNING:"
            , "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
            , "  Source locations may not work as expected."
            , ""
            , "  Please consider upgrading GHC!"
            , ""
#endif
            , "Randomized with seed 0"
            , ""
            ]

    describe "footerFormatter" $ do
      context "without failures" $ do
        let env = environment {environmentGetSuccessCount = return 1}
        it "shows summary in green if there are no failures" $ do
          formatter <- formatterIO
          let action = H.footerFormatter formatter
          interpretWith env action `shouldReturn` textBlocks [
              "Finished in 0.0000 seconds\n"
            , withSuccessColor "1 example, 0 failures\n"
            ]

      context "with pending examples" $ do
        let env = environment {environmentGetPendingCount = return 1}
        it "shows summary in yellow if there are pending examples" $ do
          formatter <- formatterIO
          let action = H.footerFormatter formatter
          interpretWith env action `shouldReturn` textBlocks [
              "Finished in 0.0000 seconds\n"
            , withPendingColor "1 example, 0 failures, 1 pending\n"
            ]

      context "with failures" $ do
        let env = environment {environmentGetFailMessages = return [undefined]}
        it "shows summary in red" $ do
          formatter <- formatterIO
          let action = H.footerFormatter formatter
          interpretWith env action `shouldReturn` textBlocks [
              "Finished in 0.0000 seconds\n"
            , withFailColor "1 example, 1 failure\n"
            ]

      context "with both failures and pending examples" $ do
        let env = environment {environmentGetFailMessages = return [undefined], environmentGetPendingCount = return 1}
        it "shows summary in red" $ do
          formatter <- formatterIO
          let action = H.footerFormatter formatter
          interpretWith env action `shouldReturn` textBlocks [
              "Finished in 0.0000 seconds\n"
            , withFailColor "2 examples, 1 failure, 1 pending\n"
            ]

    context "same as failed_examples" $ do
      failed_examplesSpec H.specdoc

failed_examplesSpec :: IO H.Formatter -> Spec
failed_examplesSpec formatter = do
  let runSpec = captureLines . H.hspecWithResult H.defaultConfig {H.configFormatter = Just formatter}

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
            addLoc e = e {H.itemLocation = Just loc}
        r <- runSpec $ H.mapSpecItem_ addLoc $ do
          H.it "foo" False
        r `shouldContain` ["  test/FooSpec.hs:23:4: ", "  1) foo"]
