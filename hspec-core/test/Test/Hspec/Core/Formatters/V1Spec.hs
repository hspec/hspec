{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Core.Formatters.V1Spec (spec) where

import           Prelude ()
import           Helper
import           Data.String
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Writer
import qualified Control.Exception as E

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Formatters.V1 as H
import qualified Test.Hspec.Core.Formatters.Monad as H (interpretWith)
import           Test.Hspec.Core.Formatters.Monad (FormatM, Environment(..), FailureRecord(..), FailureReason(..))

data ColorizedText =
    Plain String
  | Transient String
  | Info String
  | Succeeded String
  | Failed String
  | Pending String
  | Extra String
  | Missing String
  deriving (Eq, Show)

instance IsString ColorizedText where
  fromString = Plain

removeColors :: [ColorizedText] -> String
removeColors input = case input of
  Plain x : xs -> x ++ removeColors xs
  Transient _ : xs -> removeColors xs
  Info x : xs -> x ++ removeColors xs
  Succeeded x : xs -> x ++ removeColors xs
  Failed x : xs -> x ++ removeColors xs
  Pending x : xs -> x ++ removeColors xs
  Extra x : xs -> x ++ removeColors xs
  Missing x : xs -> x ++ removeColors xs
  [] -> ""

simplify :: [ColorizedText] -> [ColorizedText]
simplify input = case input of
  Plain xs : Plain ys : zs -> simplify (Plain (xs ++ ys) : zs)
  Extra xs : Extra ys : zs -> simplify (Extra (xs ++ ys) : zs)
  Missing xs : Missing ys : zs -> simplify (Missing (xs ++ ys) : zs)
  x : xs -> x : simplify xs
  [] -> []

colorize :: (String -> ColorizedText) -> [ColorizedText] -> [ColorizedText]
colorize color input = case simplify input of
  Plain x : xs -> color x : xs
  xs -> xs

interpret :: FormatM a -> IO [ColorizedText]
interpret = interpretWith environment

interpretWith :: Environment (WriterT [ColorizedText] IO) -> FormatM a -> IO [ColorizedText]
interpretWith env = fmap simplify . execWriterT . H.interpretWith env

environment :: Environment (WriterT [ColorizedText] IO)
environment = Environment {
  environmentGetSuccessCount = return 0
, environmentGetPendingCount = return 0
, environmentGetFailMessages = return []
, environmentGetFinalCount = return 0
, environmentUsedSeed = return 0
, environmentGetCPUTime = return Nothing
, environmentGetRealTime = return 0
, environmentWrite = tell . return . Plain
, environmentWriteTransient = tell . return . Transient
, environmentWithFailColor = \ action -> do
    (a, r) <- liftIO $ runWriterT action
    tell (colorize Failed r) >> return a
, environmentWithSuccessColor = \ action -> do
    (a, r) <- liftIO $ runWriterT action
    tell (colorize Succeeded r) >> return a
, environmentWithPendingColor = \ action -> do
    (a, r) <- liftIO $ runWriterT action
    tell (colorize Pending r) >> return a
, environmentWithInfoColor = \ action -> do
    (a, r) <- liftIO $ runWriterT action
    tell (colorize Info r) >> return a
, environmentUseDiff = return True
, environmentPrintTimes = return False
, environmentExtraChunk = tell . return . Extra
, environmentMissingChunk = tell . return . Missing
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
    let formatter = H.progress

    describe "exampleSucceeded" $ do
      it "marks succeeding examples with ." $ do
        interpret (H.exampleSucceeded formatter undefined undefined) `shouldReturn` [
            Succeeded "."
          ]

    describe "exampleFailed" $ do
      it "marks failing examples with F" $ do
        interpret (H.exampleFailed formatter undefined undefined undefined) `shouldReturn` [
            Failed "F"
          ]

    describe "examplePending" $ do
      it "marks pending examples with ." $ do
        interpret (H.examplePending formatter undefined undefined undefined) `shouldReturn` [
            Pending "."
          ]

  describe "specdoc" $ do
    let
      formatter = H.specdoc
      runSpec = captureLines . H.hspecWithResult H.defaultConfig {H.configFormatter = Just formatter}

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
      let action = H.failedFormatter formatter

      context "when actual/expected contain newlines" $ do
        let
          env = environment {
            environmentGetFailMessages = return [FailureRecord Nothing ([], "") (ExpectedButGot Nothing "first\nsecond\nthird" "first\ntwo\nthird")]
            }
        it "adds indentation" $ do
          (removeColors <$> interpretWith env action) `shouldReturn` unlines [
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
            ]

    describe "footerFormatter" $ do
      let action = H.footerFormatter formatter

      context "without failures" $ do
        let env = environment {environmentGetSuccessCount = return 1}
        it "shows summary in green if there are no failures" $ do
          interpretWith env action `shouldReturn` [
              "Finished in 0.0000 seconds\n"
            , Succeeded "1 example, 0 failures\n"
            ]

      context "with pending examples" $ do
        let env = environment {environmentGetPendingCount = return 1}
        it "shows summary in yellow if there are pending examples" $ do
          interpretWith env action `shouldReturn` [
              "Finished in 0.0000 seconds\n"
            , Pending "1 example, 0 failures, 1 pending\n"
            ]

      context "with failures" $ do
        let env = environment {environmentGetFailMessages = return [undefined]}
        it "shows summary in red" $ do
          interpretWith env action `shouldReturn` [
              "Finished in 0.0000 seconds\n"
            , Failed "1 example, 1 failure\n"
            ]

      context "with both failures and pending examples" $ do
        let env = environment {environmentGetFailMessages = return [undefined], environmentGetPendingCount = return 1}
        it "shows summary in red" $ do
          interpretWith env action `shouldReturn` [
              "Finished in 0.0000 seconds\n"
            , Failed "2 examples, 1 failure, 1 pending\n"
            ]

    context "same as failed_examples" $ do
      failed_examplesSpec formatter

failed_examplesSpec :: H.Formatter -> Spec
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
