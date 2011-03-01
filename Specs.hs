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
    "Step 1, write your specs",
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
    "Step 2, write your dummy function",
    "> myabs n = undefined",
    "",
    "Step 3, watch them fail",
    "> hspec specs",
    "myabs",
    " x returns the original number when given a positive input (Prelude.undefined)",
    " x returns a positive number when given a negative input (Prelude.undefined)",
    " x returns zero when given zero (Prelude.undefined)",
    "",
    "Finished in 0.0002 seconds",
    "",
    "3 examples, 3 failures",
    "",
    "Step 4, implement your specs",
    "> myabs n = if n < 0 then negate n else n", "",
    "Step 5, watch them pass",
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
    "Here's the report of hspec's specs:" ]

specs :: IO [Spec]
specs = let spec = Spec "Example" "example"
            testSpecs = [spec Success, spec (Fail ""), spec (Pending "")]
        in liftM concat $ sequence [

  describe "describe" [
    it "takes a description of what the behavior is for"
        ((=="Example") . name . head $ testSpecs),

    it "groups behaviors for what's being described"
        (all ((=="Example").name) testSpecs)
  ],
  describe "it" [
    it "takes a description of the behavior"
        (requirement (Spec "Example" "whatever" Success) == "whatever" ),

    it "takes an example of the behavior"
        (result (spec Success) == Success),

    it "can use a Bool, HUnit Test, QuickCheck property, or \"pending\" as an example"
        (True),

    it "will treat exceptions as failures"
        (HUnit.TestCase $ do
          innerSpecs <- describe "" [ it "exceptions" (True && undefined)]
          let found = pureHspec innerSpecs !! 2
          HUnit.assertEqual (unlines $ pureHspec innerSpecs) " x exceptions (Prelude.undefined)" found),

    it "allows failed examples to have details"
        (const True (Fail "details"))
  ],
  describe "Bool example" [
    it "is just an expression that evaluates to a Bool"
        (True)
  ],
  describe "HUnit example" [
    it "is specified with the HUnit \"TestCase\" data constructor"
        (HUnit.TestCase $ HUnit.assertBool "example" True),

    it "is the assumed example for IO() actions"
        (HUnit.assertBool "example" True),

    it "will show the assertion text if it fails"
        (HUnit.TestCase $ do
          innerSpecs <- describe "" [ it "fails" (HUnit.assertBool "trivial" False)]
          let found = pureHspec innerSpecs !! 2
          HUnit.assertEqual found " x fails (trivial)" found)
  ],
  describe "QuickCheck example" [
    it "is specified with the \"property\" function"
        (property $ \ b -> b || True)
  ],
  describe "pending example" [
    it "is specified with the \"pending\" function and an explanation"
        (pending "message" == Pending "message"),

    it "accepts a message to display in the report"
        (documentSpec (spec (Pending "t")) == " - example\n     # t")
  ],
  describe "hspec" [
    it "displays each thing being described as a header"
        (documentGroup [spec Success] !! 1 == "Example"),

    it "displays one row for each behavior"
        (length (documentGroup testSpecs) == 5),

    it "displays a '-' for successfully implemented behaviors"
        (documentSpec (spec Success) == " - example"),

    it "displays an 'x' for unsuccessfully implmented behaviors"
        (documentSpec (spec $ Fail "whatever") == " x example (whatever)" ),

    it "displays optional details for unsuccessfully implmented behaviors"
        (documentSpec (spec $ Fail "whatever") == " x example (whatever)" ),

    it "displays a '-' for behaviors that are pending an example"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "displays a '#' and an additional message for pending examples"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "can output to stdout"
        (True),

    it "can output to stdout in color"
        (pending "TODO in the near future, perhaps using System.Console.ANSI?"),

    it "summarizes the time it takes to finish"
        (any (=="Finished in 0.0000 seconds") (pureHspec testSpecs)),

    it "summarizes the number of examples and failures"
        (any (=="3 examples, 1 failure") (pureHspec testSpecs))
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
