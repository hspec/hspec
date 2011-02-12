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
import System.IO
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
  ar <- getArgs
  case ar of
    ["README"] -> withFile "README" WriteMode (\ h -> hPutStrLn h preable >> hHspec h specs)
    [filename] -> withFile filename WriteMode (\ h -> hHspec h specs)
    _          -> hHspec stdout specs

preable :: String
preable = unlines [ "hspec aims to be a simple, extendable, and useful tool for Behavior Driven",
                    "Development in Haskell.", "",
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
                    "> myabs n = 0",
                    "",
                    "Step 3, watch them fail",
                    "> hspec specs",
                    "myabs",
                    " x returns the original number when given a positive input",
                    " x returns a positive number when given a negative input",
                    " - returns zero when given zero",
                    "",
                    "Finished in 0.0 seconds",
                    "",
                    "3 examples, 2 failures",
                    "",
                    "Step 4, implement your requirements",
                    "> myabs n = if n < 0 then negate n else n", "",
                    "Step 5, watch them pass",
                    "> hspec specs",
                    "myabs",
                    " - returns the original number when given a positive input",
                    " - returns a positive number when given a negative input",
                    " - returns zero when given zero",
                    "",
                    "Finished in 0.0 seconds",
                    "",
                    "3 examples, 0 failures",
                    "",
                    "",
                    "",
                    "",
                    "Here's the report of hspec's specs:" ]

specs :: IO [Spec]
specs = let spec = Spec "Example" "example"
            testSpecs = [spec Success, spec Fail, spec (Pending "")]
        in liftM concat $ sequence [

  describe "describe" [
    it "groups specs into the same category"
        (all ((=="Example").name) testSpecs)
  ],
  describe "it" [
    it "contains the description of the spec"
        (requirement (spec Success) == "example" ),

    it "contains the verification that the spec was implemented"
        (result (spec Fail) == Fail ),

    it "allows a boolean expression to act as verification"
        (True),

    it "allows a verification to be pending"
        (True),

    it "allows a QuickCheck property to act as verification"
        (property quickCheckProperty),

    it "catches 'undefined' exception"
        (Fail)
  ],
  describe "hspec" [
    it "displays the spec categories as a headers"
        (documentGroup [spec Success] !! 1 == "Example"),

    it "displays one row for each spec"
        (length (documentGroup testSpecs) == 5),

    it "displays a '-' for successfully implemented specs"
        (documentSpec (spec Success) == " - example"),

    it "displays an 'x' for unsuccessfully implmented specs"
        (documentSpec (spec Fail) == " x example" ),

    it "displays a '-' for pending specs"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "displays a '#' and an additional message for pending specs"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "can output to stdout"
        True,

    it "can output to stdout in color"
        (pending "TODO in near future, perhaps using System.Console.ANSI?"),

    it "can output to an ascii text file"
        True,

    it "can output as xml"
        (pending "TODO in not so near future"),

    it "can output as html"
        (pending "TODO in not so near future"),

    it "summarizes the time it takes to finish"
        (any (=="Finished in 0.0 seconds") (pureHspec testSpecs)),

    it "summarizes the number of examples and failures"
        (any (=="3 examples, 1 failure") (pureHspec testSpecs))
  ],
  describe "quantify (internal)" [
    it "returns an amount and a description given an amount and description"
        (quantify (1::Int) "thing" == "1 thing"),

    it "returns a singular description given the number 1"
        (quantify (1::Int) "thing" == "1 thing"),

    it "returns a plural description given a number greater than 1"
        (quantify (2::Int) "thing" == "2 things"),

    it "returns a plural description given the number 0"
        (quantify (0::Int) "thing" == "0 things"),

    it "handles describing the plural of words that end with an 'x'"
        (pending "no need for this yet")
  ]]

quickCheckProperty :: Int -> Bool
quickCheckProperty _ = True
