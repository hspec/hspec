{-# LANGUAGE CPP #-}
module Test.Hspec.FormattersSpec (main, spec) where

import           Test.Hspec.Meta
import           Data.List

import           Util (capture)
import qualified System.IO.Silently as S
import           Data.Char
import qualified Test.Hspec as H
import qualified Test.Hspec.Core as H (Result(..))
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Formatters as H

#ifndef mingw32_HOST_OS
import           System.Console.ANSI
#endif

main :: IO ()
main = hspec spec

testSpec :: H.Spec
testSpec = do
  H.describe "Example" $ do
    H.it "success"    (H.Success)
    H.it "fail 1"     (H.Fail "fail message")
    H.it "pending"    (H.pending "pending message")
    H.it "fail 2"     (H.Fail "")
    H.it "exceptions" (undefined :: H.Result)
    H.it "fail 3"     (H.Fail "")

-- replace times in summary with zeroes
normalizeSummary :: [String] -> [String]
normalizeSummary xs = map f xs
  where
    f x | "Finished in " `isPrefixOf` x = map g x
        | otherwise = x
    g x | isNumber x = '0'
        | otherwise  = x

spec :: Spec
spec = do
  describe "silent" $ do
    let runSpec = fmap fst . S.capture . H.hspecWith H.defaultConfig {H.configFormatter = H.silent}
    it "produces no output" $ do
      runSpec testSpec `shouldReturn` ""

  describe "failed_examples" $ do
    failed_examplesSpec H.failed_examples

  describe "progress" $ do
    let runSpec = capture . H.hspecWith H.defaultConfig {H.configFormatter = H.progress}

    it "produces '..F...FF.F' style output" $ do
      r <- runSpec testSpec
      head r `shouldBe` ".F.FFF"

    context "same as failed_examples" $ do
      failed_examplesSpec H.progress

  describe "specdoc" $ do
    let runSpec = capture . H.hspecWith H.defaultConfig {H.configFormatter = H.specdoc}

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
        , "    - is associative"
        , ""
        , "  mempty"
        , "    - is a left identity"
        , "    - is a right identity"
        , ""
        , "Maybe as a Monoid"
        , "  mappend"
        , "    - is associative"
        , ""
        , "  mempty"
        , "    - is a left identity"
        , "    - is a right identity"
        , ""
        , "Finished in 0.0000 seconds, used 0.0000 seconds of CPU time"
        , ""
        , "6 examples, 0 pending, 0 failures"
        ]

    it "prints an empty line before each group" $ do
      r <- runSpec $ do
        H.describe "foo" $ do
          H.it "example 1" True
          H.it "example 2" True
          H.describe "bar" $ do
            H.it "example 3" True
            H.it "example 4" True
      normalizeSummary r `shouldBe` [
          ""
        , "foo"
        , "  - example 1"
        , "  - example 2"
        , ""
        , "  bar"
        , "    - example 3"
        , "    - example 4"
        , ""
        , "Finished in 0.0000 seconds, used 0.0000 seconds of CPU time"
        , ""
        , "4 examples, 0 pending, 0 failures"
        ]

    it "prints an empty line after each group" $ do
      r <- runSpec $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "example 1" True
            H.it "example 2" True
          H.it "example 3" True
          H.it "example 4" True
      normalizeSummary r `shouldBe` [
          ""
        , "foo"
        , "  bar"
        , "    - example 1"
        , "    - example 2"
        , ""
        , "  - example 3"
        , "  - example 4"
        , ""
        , "Finished in 0.0000 seconds, used 0.0000 seconds of CPU time"
        , ""
        , "4 examples, 0 pending, 0 failures"
        ]

    it "outputs an empty line at the beginning (even for non-nested specs)" $ do
      r <- runSpec $ do
        H.it "example 1" True
        H.it "example 2" True
      normalizeSummary r `shouldBe` [
          ""
        , "- example 1"
        , "- example 2"
        , ""
        , "Finished in 0.0000 seconds, used 0.0000 seconds of CPU time"
        , ""
        , "2 examples, 0 pending, 0 failures"
        ]

    it "displays a row for each successfull, failed, or pending example" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "  - fail 1 FAILED [1]")
      r `shouldSatisfy` any (== "  - success")

    it "displays a '#' with an additional message for pending examples" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "     # PENDING: pending message")

    it "outputs failed examples in red, pending in yellow, and successful in green" $ do
      pending

    context "same as failed_examples" $ do
      failed_examplesSpec H.progress

failed_examplesSpec :: H.Formatter -> Spec
failed_examplesSpec formatter = do
  let runSpec = capture . H.hspecWith H.defaultConfig {H.configFormatter = formatter}

  it "summarizes the time it takes to finish" $ do
    r <- runSpec (return ())
    normalizeSummary r `shouldSatisfy` any (== "Finished in 0.0000 seconds, used 0.0000 seconds of CPU time")

  context "displays a detailed list of failures" $ do
    it "prints all requirements that are not met" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "1) Example fail 1 FAILED")

    it "prints the exception type for requirements that fail due to an uncaught exception" $ do
      r <- runSpec $ do
        H.it "foobar" (undefined :: Bool)
      r `shouldSatisfy` isInfixOf [
          "1) foobar FAILED (uncaught exception)"
        , "GHC.Exception.ErrorCall (Prelude.undefined)"
        ]

    it "prints all descriptions when a nested requirement fails" $ do
      r <- runSpec $
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" False
      r `shouldSatisfy` any (== "1) foo.bar baz FAILED")

  it "summarizes the number of examples and failures" $ do
    r <- runSpec testSpec
    r `shouldSatisfy` any (== "6 examples, 1 pending, 4 failures")

  -- Windows has no support for ANSI escape codes.  The Console API is used for
  -- colorized output, hence the following tests do not work on Windows.
#ifndef mingw32_HOST_OS
  it "shows summary in green if there are no failures" $ do
    r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} $ do
      H.it "foobar" True
    r `shouldSatisfy` any (== (green ++ "1 example, 0 pending, 0 failures" ++ reset))

  it "shows summary in yellow if there are pending examples" $ do
    r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} $ do
      H.it "foobar" H.pending
    r `shouldSatisfy` any (== (yellow ++ "1 example, 1 pending, 0 failures" ++ reset))

  it "shows summary in red if there are failures" $ do
    r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} $ do
      H.it "foobar" False
    r `shouldSatisfy` any (== (red ++ "1 example, 0 pending, 1 failure" ++ reset))

  it "shows summary in red if there are both failures and pending examples" $ do
    r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} $ do
      H.it "foo" False
      H.it "bar" H.pending
    r `shouldSatisfy` any (== (red ++ "2 examples, 1 pending, 1 failure" ++ reset))
  where
    green  = setSGRCode [SetColor Foreground Dull Green]
    yellow = setSGRCode [SetColor Foreground Dull Yellow]
    red    = setSGRCode [SetColor Foreground Dull Red]
    reset  = setSGRCode [Reset]
#endif
