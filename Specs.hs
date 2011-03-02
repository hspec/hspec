-----------------------------------------------------------------------------
--
-- Module      :  Specs
-- Copyright   :  (c) Trystan Spangler 2011
-- License     :  modified BSD
--
-- Maintainer  : trystan.s@comcast.net
-- Stability   : experimental
-- Portability : portable
--
-- |
--
-----------------------------------------------------------------------------

module Specs where

import Test.Hspec.Internal
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import System.IO
import System.Environment
import System.Exit
import Control.Monad (liftM)
import qualified Test.HUnit as HUnit

main :: IO ()
main = do
  ar <- getArgs
  b <- case ar of
    ["README"] -> withFile "README" WriteMode (\ h -> hPutStrLn h preable >> hHspec h specs)
    [filename] -> withFile filename WriteMode (\ h -> hHspec h specs)
    _          -> hspecB specs
  exitWith $ toExitCode b

preable :: String
preable = unlines [
    "hspec aims to be a simple, extendable, and useful tool for Behavior Driven Development in Haskell.", "",
    "",
    "Step 1, write descriptions and examples of your desired behavior",
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
    "Step 3, watch your examples fail",
    "> hspec specs",
    "myabs",
    " x returns the original number when given a positive input [1]",
    " x returns a positive number when given a negative input [2]",
    " x returns zero when given zero [3]",
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
    "Step 4, implement your desired behavior",
    "> myabs n = if n < 0 then negate n else n",
    "",
    "Step 5, watch your examples pass",
    "> hspec specs",
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

specs :: IO [Spec]
specs = do
  exampleSpecs <- describe "Example" [
    it "pass" (Success),
    it "fail 1" (Fail "fail message"),
    it "pending" (Pending "pending message"),
    it "fail 2" (HUnit.assertEqual "assertEqual test" 1 (2::Int)),
    it "exceptions" (undefined :: Bool)]

  let report = pureHspec exampleSpecs

  -- mapM_ putStrLn $ ["-- START example specs --"] ++ report ++ ["-- END example specs --"]

  -- the real specs
  liftM concat $ sequence [

    describe "the \"describe\" function" [
        it "takes a description of what the behavior is for"
            ((=="Example") . name . head $ exampleSpecs),

        it "groups behaviors for what's being described"
            (all ((=="Example").name) exampleSpecs)
    ],
    describe "the \"it\" function" [
        it "takes a description of a desired behavior"
            (requirement (Spec "Example" "whatever" Success) == "whatever" ),

        it "takes an example of that behavior"
            (result (Spec "Example" "whatever" Success) == Success),

        it "can use a Bool, HUnit Test, QuickCheck property, or \"pending\" as an example"
            (True),

        it "will treat exceptions as failures"
            (any (==" x exceptions [3]") report)
    ],
    describe "the \"hspec\" function" [
        it "displays a header for each thing being described"
            (any (=="Example") report),

        it "displays one row for each behavior"
            (HUnit.assertEqual "" 23 (length report)),
            -- 19 = group header + behaviors + blank lines + time + error messsages + pending message + example/failures footer

        it "displays a '-' for successfull examples"
            (any (==" - pass") report),

        it "displays an 'x' for failed examples"
            (any (==" x fail 1 [1]") report),

        it "displays a list of failed examples"
            (any (=="1) Example fail 1 FAILED") report),

        it "displays available details for failed examples"
            (any (=="fail message") report),

        it "displays a '-' for pending examples"
            (any (==" - pending") report ),

        it "displays a '#' and an additional message for pending examples"
            (any (=="     # pending message") report ),

        it "summarizes the time it takes to finish"
            (any (=="Finished in 0.0000 seconds") (pureHspec exampleSpecs)),

        it "summarizes the number of examples and failures"
            (any (=="5 examples, 3 failures") (pureHspec exampleSpecs)),

        it "outputs to stdout"
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

        it "will show the assertion text if available (e.g. assertBool)"
            (HUnit.TestCase $ do
              innerSpecs <- describe "" [ it "" (HUnit.assertBool "trivial" False)]
              let innerReport = pureHspec innerSpecs
              HUnit.assertBool "should find assertion text" $ any (=="trivial") innerReport),

        it "will show the expected and actual values if available (e.g. assertEqual)"
            (HUnit.TestCase $ do
              innerSpecs <- describe "" [ it "" (HUnit.assertEqual "trivial" (1::Int) 2)]
              let innerReport = pureHspec innerSpecs
              HUnit.assertBool "should find assertion text" $ any (=="trivial") innerReport
              HUnit.assertBool "should find 'expected: 1'" $ any (=="expected: 1") innerReport
              HUnit.assertBool "should find ' but got: 2'" $ any (==" but got: 2") innerReport)
    ],
    describe "QuickCheck property as an example" [
        it "is specified with the \"property\" function"
            (property $ \ b -> b || True)
        ],
    describe "pending as an example" [
        it "is specified with the \"pending\" function and an explanation"
            (pending "message" == Pending "message"),

        it "accepts a message to display in the report"
            (any (== "     # pending message") report)
        ],
    describe "hspecB" [
        it "returns true if no examples failed"
            (HUnit.TestCase $ do
              ss <- describe "" [it "" Success]
              HUnit.assertEqual "no errors" True (snd $ pureHspecB ss)),

        it "returns false if any examples failed"
            (HUnit.TestCase $ do
              ss <- describe "" [it "" (Fail "test")]
              HUnit.assertEqual "one error" False (snd $ pureHspecB ss))
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
