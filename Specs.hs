{-# LANGUAGE StandaloneDeriving #-}
module Specs where

import Test.Hspec
import Test.Hspec.Runner (hHspecWithFormat, toExitCode)
import Test.Hspec.Core (Spec(..), Result(..), quantify, failedCount, evaluateExample)
import Test.Hspec.Formatters
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.HUnit
import System.IO
import System.IO.Silently
import System.Environment
import System.Exit (exitWith)
import Data.List (isPrefixOf)
import qualified Test.HUnit as HUnit

deriving instance Show Result

main :: IO ()
main = do
  ar <- getArgs
  specs' <- specs
  ss <- case ar of
    ["README"] -> withFile "README" WriteMode (\ h -> hPutStrLn h preamble >> hHspec h specs')
    [filename] -> withFile filename WriteMode (\ h -> hHspec h specs')
    _          -> hspec specs'
  exitWith $ toExitCode (failedCount ss == 0)

preamble :: String
preamble = unlines [
    "hspec aims to be a simple, extendable, and useful tool for Behavior Driven Development in Haskell.", "",
    "",
    "Step 1, write descriptions and examples of your desired behavior",
    "> module Myabs where",
    ">",
    "> import Test.Hspec",
    ">",
    "> specs :: Specs",
    "> specs = describe \"myabs\" [",
    ">   it \"returns the original number when given a positive input\"",
    ">     (myabs 1 == 1),",
    "> ",
    ">   it \"returns a positive number when given a negative input\"",
    ">     (myabs (-1) == 1),",
    "> ",
    ">   it \"returns zero when given zero\"",
    ">     (myabs 0 == 0)",
    ">   ]",
    "",
    "Step 2, write whatever you are describing",
    "> myabs n = undefined",
    "",
    "Step 3, watch your examples fail with red text by running from the .hs file itself",
    "> main = hspec specs",
    "",
    "myabs",
    " - returns the original number when given a positive input FAILED [1]",
    " - returns a positive number when given a negative input FAILED [2]",
    " - returns zero when given zero FAILED [3]",
    "",
    "1) myabs returns the original number when given a positive input FAILED",
    "Prelude.undefined",
    "",
    "2) myabs returns a positive number when given a negative input FAILED",
    "Prelude.undefined",
    "",
    "3) myabs returns zero when given zero FAILED",
    "Prelude.undefined",
    "",
    "Finished in 0.0002 seconds",
    "",
    "3 examples, 3 failures",
    "",
    "",
    "Specs can also be run from the command line using the hspec program",
    "  $ hspec myabs.hs",
    "",
    "Step 4, implement your desired behavior",
    "> myabs n = if n < 0 then negate n else n",
    "",
    "Step 5, watch your examples succeed with green text when rerun",
    "myabs",
    " - returns the original number when given a positive input",
    " - returns a positive number when given a negative input",
    " - returns zero when given zero",
    "",
    "Finished in 0.0000 seconds",
    "",
    "3 examples, 0 failures",
    "",
    "",
    "",
    "",
    "Here's the report of hspec's behavior:" ]

specs :: IO Specs
specs = do
  let testSpecs = [describe "Example" [
          it "success" (Success),
          it "fail 1" (Fail "fail message"),
          it "pending" (Pending "pending message"),
          it "fail 2" (HUnit.assertEqual "assertEqual test" 1 (2::Int)),
          it "exceptions" (undefined :: Bool),
          it "quickcheck" (property $ \ i -> i == (i+1::Integer))]
          ]

  (reportContents, exampleSpecs)     <- capture $ hHspecWithFormat specdoc         False stdout testSpecs
  (silentReportContents, _)          <- capture $ hHspecWithFormat silent          False stdout testSpecs
  (progressReportContents, _)        <- capture $ hHspecWithFormat progress        False stdout testSpecs
  (failed_examplesReportContents, _) <- capture $ hHspecWithFormat failed_examples False stdout testSpecs

  let report = lines reportContents

  return [
    describe "the \"describe\" function" [
        it "takes a description of what the behavior is for" $
            case exampleSpecs of
              [SpecGroup "Example" _] -> True
              _ -> False
        ,
        it "groups behaviors for what's being described" $
            case exampleSpecs of
              [SpecGroup _ xs] -> length xs == 6
              _ -> False
        ,
        describe "a nested description" [
            it "has it's own specs"
                (True)
        ]
    ],
    describe "the \"it\" function" [
        it "takes a description of a desired behavior" $
            case it "whatever" Success of
              SpecExample requirement _ -> requirement == "whatever"
              _ -> False
        ,
        it "takes an example of that behavior" $ do
            case it "whatever" Success of
              SpecExample _ example -> do
                r <- evaluateExample example
                r @?= Success
              SpecGroup _ _ -> assertFailure "unexpected SpecGroup"
        ,
        it "can use a Bool, HUnit Test, QuickCheck property, or \"pending\" as an example"
            (True),

        it "will treat exceptions as failures"
            (any (==" - exceptions FAILED [3]") report)
    ],
    describe "the \"hspec\" function" [
        it "displays a header for each thing being described"
            (any (=="Example") report),

        it "displays one row for each behavior"
            (HUnit.assertEqual "" 29 (length report)),

        it "displays a row for each successfull, failed, or pending example"
            (any (==" - success") report && any (==" - fail 1 FAILED [1]") report),

        it "displays a detailed list of failed examples"
            (any (=="1) Example fail 1 FAILED") report),

        it "displays a '#' with an additional message for pending examples"
            (any (=="     # pending message") report ),

        it "summarizes the time it takes to finish"
            (any ("Finished in " `isPrefixOf`) report),

        it "summarizes the number of examples and failures"
            (any (=="6 examples, 4 failures") report),

        it "outputs failed examples in red, pending in yellow, and successful in green"
            (True)
    ],
    describe "Bool as an example" [
        it "is just an expression that evaluates to a Bool"
            (True)
    ],
    describe "HUnit TestCase as an example" [
        it "is specified with the HUnit \"TestCase\" data constructor"
            (HUnit.TestCase $ HUnit.assertBool "example" True),

        it "is the assumed example for IO() actions"
            (HUnit.assertBool "example" True),

        it "will show the failed assertion text if available (e.g. assertBool)"
            (HUnit.TestCase $ do
              (innerReport, _) <- capture $ hspec $ [describe "" [ it "" (HUnit.assertBool "trivial" False)]]
              HUnit.assertBool "should find assertion text" $ any (=="trivial") (lines innerReport)),

        it "will show the failed assertion expected and actual values if available (e.g. assertEqual)"
            (HUnit.TestCase $ do
              (innerReportContents, _) <- capture $ hspec $ [describe "" [ it "" (HUnit.assertEqual "trivial" (1::Int) 2)]]
              let innerReport = lines innerReportContents
              HUnit.assertBool "should find assertion text" $ any (=="trivial") innerReport
              HUnit.assertBool "should find 'expected: 1'" $ any (=="expected: 1") innerReport
              HUnit.assertBool "should find ' but got: 2'" $ any (==" but got: 2") innerReport)
    ],
    describe "QuickCheck property as an example" [
        it "is specified with the \"property\" function"
            (property $ \ b -> b || True),

        it "will show what falsified it"
            (any (=="0") report)
    ],
    describe "pending as an example" [
        it "is specified with the \"pending\" function and an explanation"
            (pending "message" == Pending "message"),

        it "accepts a message to display in the report"
            (any (== "     # pending message") report)
    ],
    describe "the \"hHspecWithFormat\" function" [
        it "can use the \"silent\" formatter to show no output"
            (null silentReportContents),

        it "can use the \"progress\" formatter to show '..F...FF.F' style output"
            (HUnit.assertEqual "" ".F.FFF" (head $ lines progressReportContents)),

        it "can use the \"specdoc\" formatter to show all examples (default)"
            (HUnit.assertEqual "" "Example" (lines reportContents !! 1)),

        it "can use the \"failed_examples\" formatter to show only failed examples"
            (HUnit.assertEqual "" "1) Example fail 1 FAILED" (lines failed_examplesReportContents !! 1))
    ],
    describe "quantify (an internal function)" [
        it "returns an amount and a word given an amount and word"
            (quantify (1::Int) "thing" == "1 thing"),

        it "returns a singular word given the number 1"
            (quantify (1::Int) "thing" == "1 thing"),

        it "returns a plural word given a number greater than 1"
            (quantify (2::Int) "thing" == "2 things"),

        it "returns a plural word given the number 0"
            (quantify (0::Int) "thing" == "0 things")
    ]]
