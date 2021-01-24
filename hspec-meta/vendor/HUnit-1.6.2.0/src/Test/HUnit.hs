-- | HUnit is a unit testing framework for Haskell, inspired by the JUnit tool
-- for Java. This guide describes how to use HUnit, assuming you are familiar
-- with Haskell, though not necessarily with JUnit.
--
-- In the Haskell module where your tests will reside, import module
-- @Test.HUnit@:
--
-- @
--    import Test.HUnit
-- @
--
--  Define test cases as appropriate:
--
-- @
--    test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
--    test2 = TestCase (do (x,y) <- partA 3
--                         assertEqual "for the first result of partA," 5 x
--                         b <- partB y
--                         assertBool ("(partB " ++ show y ++ ") failed") b)
-- @
--
-- Name the test cases and group them together:
--
-- @
--    tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
-- @
--
-- Run the tests as a group. At a Haskell interpreter prompt, apply the function
-- @runTestTT@ to the collected tests. (The /TT/ suggests /T/ext orientation
-- with output to the /T/erminal.)
--
-- @
--    \> runTestTT tests
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    \>
-- @
--
-- If the tests are proving their worth, you might see:
--
-- @
--    \> runTestTT tests
--    ### Failure in: 0:test1
--    for (foo 3),
--    expected: (1,2)
--     but got: (1,3)
--    Cases: 2  Tried: 2  Errors: 0  Failures: 1
--    \>
-- @
--
-- You can specify tests even more succinctly using operators and overloaded
-- functions that HUnit provides:
--
-- @
--    tests = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
--                   "test2" ~: do (x, y) <- partA 3
--                                 assertEqual "for the first result of partA," 5 x
--                                 partB y \@? "(partB " ++ show y ++ ") failed" ]
-- @
--
-- Assuming the same test failures as before, you would see:
--
-- @
--    \> runTestTT tests
--    ### Failure in: 0:test1:(foo 3)
--    expected: (1,2)
--     but got: (1,3)
--    Cases: 2  Tried: 2  Errors: 0  Failures: 1
--    \>
-- @

module Test.HUnit
(
  module Test.HUnit.Base,
  module Test.HUnit.Text
)
where

import Test.HUnit.Base
import Test.HUnit.Text

