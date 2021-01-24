module HUnitTestExtended (extendedTests) where

import Test.HUnit
import HUnitTestBase

extendedTests :: Test
extendedTests = test [
    "div by 0" ~:
        expectError "divide by zero" (TestCase ((3 `div` 0 :: Integer) `seq` return ())),

    "list ref out of bounds" ~:
        expectUnspecifiedError (TestCase ([1 .. 4 :: Integer] !! 10 `seq` return ())),

     "error" ~:
        expectUnspecifiedError (TestCase (error "error")),

    "tail []" ~:
        expectUnspecifiedError (TestCase (tail [] `seq` return ()))
    ]
