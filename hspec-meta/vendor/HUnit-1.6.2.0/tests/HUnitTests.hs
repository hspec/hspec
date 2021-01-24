-- HUnitTests.hs
--
-- This file is an entry point for running all of the tests.

module Main (main) where

import System.Exit

import Test.HUnit
import HUnitTestBase
import HUnitTestExtended
import TerminalTest
import Example ()

main :: IO ()
main = do
    counts2 <- runTestTT (test [
            baseTests,
            extendedTests,
            terminalTests
            ])
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
