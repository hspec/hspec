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
import System.IO
import System.Environment

main :: IO ()
main = do
  ar <- getArgs
  case ar of
    [filename] -> withFile filename WriteMode (\ h -> hHspec h specs)
    _          -> hHspec stdout specs

specs :: [Spec]
specs = let spec = Spec "Example" "example"
            testSpecs = describe "group" [it "a" False, it "b" False, it "c" False]
        in
  describe "describe" [
    it "groups specs into the same category"
        (all ((=="group").name) testSpecs)
  ]
  ++ describe "it" [
    it "contains the description of the spec"
        (requirement (spec Success) == "example" ),

    it "contains the validation that the spec was implemented"
        (result (spec Fail) == Fail )
  ]
  ++ describe "hspec" [
    it "displays the spec categories as a headers"
        (documentGroup [spec Success] !! 1 == "Example"),

    it "displays one row for each spec"
        (length (documentGroup testSpecs) == 5),

    it "displays a '-' for successfully implemented specs"
        (documentSpec (spec Success) == " - example"),

    it "displays a 'x' for unsuccessfully implmented specs"
        (documentSpec (spec Fail) == " x example" ),

    it "displays a '-' for pending specs"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "displays a '#' and an additional message for pending specs"
        (documentSpec (spec (Pending "pending")) == " - example\n     # pending" ),

    it "can output to stdout"
        True,

    it "can output to stdout in color"
        (pending "near future, perhaps using System.Console.ANSI?"),

    it "can output to an ascii text file"
        (pending "near future"),

    it "can output as xml"
        (pending "not so near future"),

    it "can output as html"
        (pending "not so near future"),

    it "summarizes the time it takes to finish"
        (any (=="Finished in 0.0 seconds") (hspec testSpecs)),

    it "summarizes the number of examples and failures"
        (any (=="3 examples, 3 failures") (hspec testSpecs))
  ]
  ++ describe "quantify (internal)" [
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
  ]

