# HUnit User's Guide

HUnit is a unit testing framework for Haskell, inspired by the JUnit tool for Java. This
guide describes how to use HUnit, assuming you are familiar with Haskell, though not
necessarily with JUnit. You can obtain HUnit, including this guide, at
[https://github.com/hspec/HUnit](https://github.com/hspec/HUnit)

## Introduction
A test-centered methodology for software development is most effective when tests are
easy to create, change, and execute. The [JUnit](www.junit.org) tool
pioneered support for test-first development in [Java](http://java.sun.com).
HUnit is an adaptation of JUnit to Haskell, a general-purpose, purely functional
programming language. (To learn more about Haskell, see [www.haskell.org](http://www.haskell.org)).

With HUnit, as with JUnit, you can easily create tests, name them, group them into
suites, and execute them, with the framework checking the results automatically. Test
specification in HUnit is even more concise and flexible than in JUnit, thanks to the
nature of the Haskell language. HUnit currently includes only a text-based test
controller, but the framework is designed for easy extension. (Would anyone care to
write a graphical test controller for HUnit?)

The next section helps you get started using HUnit in simple ways. Subsequent sections
give details on [writing tests](#writing-tests) and [running tests](#running-tests).
The document concludes with a section describing HUnit's [constituent files](#constituent-files)
and a section giving [references](#references) to further information.

## Getting Started

In the Haskell module where your tests will reside, import module `Test.HUnit`:

```haskell
import Test.HUnit
```

Define test cases as appropriate:

```haskell
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
```

Name the test cases and group them together:

```haskell
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
```

Run the tests as a group. At a Haskell interpreter prompt, apply the
function `runTestTT` to the collected tests. (The `TT` suggests
**T**ext orientation with output to the **T**erminal.)

```haskell
> runTestTT tests
Cases: 2  Tried: 2  Errors: 0  Failures: 0
>
```

If the tests are proving their worth, you might see:

```haskell
> runTestTT tests
### Failure in: 0:test1
for (foo 3),
expected: (1,2)
 but got: (1,3)
Cases: 2  Tried: 2  Errors: 0  Failures: 1
>
```

Isn't that easy?

You can specify tests even more succinctly using operators and
overloaded functions that HUnit provides:

```haskell
tests = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
               "test2" ~: do (x, y) <- partA 3
                             assertEqual "for the first result of partA," 5 x
                             partB y @? "(partB " ++ show y ++ ") failed" ]
```

Assuming the same test failures as before, you would see:

```haskell
> runTestTT tests
### Failure in: 0:test1:(foo 3)
expected: (1,2)
 but got: (1,3)
Cases: 2  Tried: 2  Errors: 0  Failures: 1
>
```

## Writing Tests

Tests are specified compositionally. [Assertions](#assertions) are
combined to make a [test case](#test-case), and test cases are combined
into [tests](#tests). HUnit also provides [advanced
features](#advanced-features) for more convenient test specification.

### Assertions

 The basic building block of a test is an **assertion**.

```haskell
type Assertion = IO ()
```

An assertion is an `IO` computation that always produces a void result. Why is an assertion an `IO` computation? So that programs with real-world side effects can be tested. How does an assertion assert anything if it produces no useful result? The answer is that an assertion can signal failure by calling `assertFailure`.

```haskell
assertFailure :: String -> Assertion
assertFailure msg = ioError (userError ("HUnit:" ++ msg))
```

`(assertFailure msg)` raises an exception. The string argument identifies the
 failure. The failure message is prefixed by "`HUnit:`" to mark it as an HUnit
 assertion failure message. The HUnit test framework interprets such an exception as
 indicating failure of the test whose execution raised the exception. (Note: The details
 concerning the implementation of `assertFailure` are subject to change and should
 not be relied upon.)

`assertFailure` can be used directly, but it is much more common to use it
 indirectly through other assertion functions that conditionally assert failure.

```haskell
assertBool :: String -> Bool -> Assertion
assertBool msg b = unless b (assertFailure msg)

assertString :: String -> Assertion
assertString s = unless (null s) (assertFailure s)

assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqual preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual
```

With `assertBool` you give the assertion condition and failure message separately.
 With `assertString` the two are combined. With `assertEqual` you provide a
 "preface", an expected value, and an actual value; the failure message shows the two
 unequal values and is prefixed by the preface. Additional ways to create assertions are
 described later under [Advanced Features](#advanced-features)

Since assertions are `IO` computations, they may be combined--along with other
     `IO` computations--using `(>>=)`, `(>>)`, and the `do`
 notation. As long as its result is of type `(IO ())`, such a combination
 constitutes a single, collective assertion, incorporating any number of constituent
 assertions. The important features of such a collective assertion are that it fails if
 any of its constituent assertions is executed and fails, and that the first constituent
 assertion to fail terminates execution of the collective assertion. Such behavior is
 essential to specifying a test case.

### Test Case

A **test case** is the unit of test execution. That is, distinct test cases are
 executed independently. The failure of one is independent of the failure of any other.

A test case consists of a single, possibly collective, assertion. The possibly multiple
 constituent assertions in a test case's collective assertion are **not** independent.
 Their interdependence may be crucial to specifying correct operation for a test. A test
 case may involve a series of steps, each concluding in an assertion, where each step
 must succeed in order for the test case to continue. As another example, a test may
 require some "set up" to be performed that must be undone ("torn down" in JUnit
 parlance) once the test is complete. In this case, you could use Haskell's
     `IO.bracket` function to achieve the desired effect.

You can make a test case from an assertion by applying the `TestCase` constructor.
 For example, `(TestCase (return ()))` is a test case that never
 fails, and `(TestCase (assertEqual "for x," 3 x))`
 is a test case that checks that the value of `x` is 3.  Additional ways
 to create test cases are described later under [Advanced Features](#advanced-eatures).

### Tests

As soon as you have more than one test, you'll want to name them to tell them apart. As
 soon as you have more than several tests, you'll want to group them to process them more
 easily. So, naming and grouping are the two keys to managing collections of tests.

In tune with the "composite" design pattern [1], a
 **test** is defined as a package of test cases. Concretely, a test is either a single
 test case, a group of tests, or either of the first two identified by a label.

```haskell
data Test = TestCase Assertion
          | TestList [Test]
          | TestLabel String Test
```

There are three important features of this definition to note:


* A `TestList` consists of a list of tests rather than a list of test cases.
   This means that the structure of a `Test` is actually a tree. Using a
   hierarchy helps organize tests just as it helps organize files in a file system.
* A `TestLabel` is attached to a test rather than to a test case. This means
   that all nodes in the test tree, not just test case (leaf) nodes, can be labeled.
   Hierarchical naming helps organize tests just as it helps organize files in a file
   system.
* A `TestLabel` is separate from both `TestCase` and `TestList`.
   This means that labeling is optional everywhere in the tree. Why is this a good
   thing? Because of the hierarchical structure of a test, each constituent test case
   is uniquely identified by its path in the tree, ignoring all labels. Sometimes a
   test case's path (or perhaps its subpath below a certain node) is a perfectly
   adequate "name" for the test case (perhaps relative to a certain node). In this
   case, creating a label for the test case is both unnecessary and inconvenient.


The number of test cases that a test comprises can be computed with `testCaseCount`.

```haskell
testCaseCount :: Test -> Int
```

As mentioned above, a test is identified by its **path** in the test hierarchy.

```haskell
data Node  = ListItem Int | Label String
  deriving (Eq, Show, Read)

type Path = [Node]    -- Node order is from test case to root.
```

Each occurrence of `TestList` gives rise to a `ListItem` and each
 occurrence of `TestLabel` gives rise to a `Label`. The `ListItem`s
 by themselves ensure uniqueness among test case paths, while the `Label`s allow
 you to add mnemonic names for individual test cases and collections of them.

Note that the order of nodes in a path is reversed from what you might expect: The first
 node in the list is the one deepest in the tree. This order is a concession to
 efficiency: It allows common path prefixes to be shared.

The paths of the test cases that a test comprises can be computed with
 `testCasePaths`. The paths are listed in the order in which the corresponding
 test cases would be executed.

```haskell
testCasePaths :: Test -> [Path]
```

The three variants of `Test` can be constructed simply by applying
 `TestCase`, `TestList`, and `TestLabel` to appropriate arguments.
 Additional ways to create tests are described later under [Advanced Features](#advanced-features).

The design of the type `Test` provides great conciseness, flexibility, and
 convenience in specifying tests. Moreover, the nature of Haskell significantly augments
 these qualities:

* Combining assertions and other code to construct test cases is easy with the
    `IO` monad.
* Using overloaded functions and special operators (see below), specification of
    assertions and tests is extremely compact.
* Structuring a test tree by value, rather than by name as in JUnit, provides for more
    convenient, flexible, and robust test suite specification. In particular, a test
    suite can more easily be computed "on the fly" than in other test frameworks.
* Haskell's powerful abstraction facilities provide unmatched support for test
    refactoring.

### Advanced Features

HUnit provides additional features for specifying assertions and tests more conveniently
 and concisely. These facilities make use of Haskell type classes.

The following operators can be used to construct assertions.

```haskell
infix 1 @?, @=?, @?=

(@?) :: (AssertionPredicable t) => t -> String -> Assertion
pred @? msg = assertionPredicate pred >>= assertBool msg

(@=?) :: (Eq a, Show a) => a -> a -> Assertion
expected @=? actual = assertEqual "" expected actual

(@?=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?= expected = assertEqual "" expected actual
```

You provide a boolean condition and failure message separately to `(@?)`, as for
     `assertBool`, but in a different order. The `(@=?)` and `(@?=)`
 operators provide shorthands for `assertEqual` when no preface is required. They
 differ only in the order in which the expected and actual values are provided. (The
 actual value--the uncertain one--goes on the "?" side of the operator.)

The `(@?)` operator's first argument is something from which an assertion
 predicate can be made, that is, its type must be `AssertionPredicable`.

```haskell
type AssertionPredicate = IO Bool

class AssertionPredicable t
 where assertionPredicate :: t -> AssertionPredicate

instance AssertionPredicable Bool
 where assertionPredicate = return

instance (AssertionPredicable t) => AssertionPredicable (IO t)
 where assertionPredicate = (>>= assertionPredicate)
```

The overloaded `assert` function in the `Assertable` type class constructs
 an assertion.

```haskell
class Assertable t
 where assert :: t -> Assertion

instance Assertable ()
 where assert = return

instance Assertable Bool
 where assert = assertBool ""

instance (ListAssertable t) => Assertable [t]
 where assert = listAssert

instance (Assertable t) => Assertable (IO t)
 where assert = (>>= assert)
```

The `ListAssertable` class allows `assert` to be applied to `[Char]`
 (that is, `String`).

```haskell
class ListAssertable t
 where listAssert :: [t] -> Assertion

instance ListAssertable Char
 where listAssert = assertString
```

With the above declarations, `(assert ())`,
 `(assert True)`, and `(assert "")` (as well as
 `IO` forms of these values, such as `(return ())`) are all
 assertions that never fail, while `(assert False)` and
     `(assert "some failure message")` (and their
     `IO` forms) are assertions that always fail. You may define additional
 instances for the type classes `Assertable`, `ListAssertable`, and
     `AssertionPredicable` if that should be useful in your application.

The overloaded `test` function in the `Testable` type class constructs a
 test.

```haskell
class Testable t
 where test :: t -> Test

instance Testable Test
 where test = id

instance (Assertable t) => Testable (IO t)
 where test = TestCase . assert

instance (Testable t) => Testable [t]
 where test = TestList . map test
```

The `test` function makes a test from either an `Assertion` (using
     `TestCase`), a list of `Testable` items (using `TestList`), or
 a `Test` (making no change).

The following operators can be used to construct tests.

```haskell
infix  1 ~?, ~=?, ~?=
infixr 0 ~:

(~?) :: (AssertionPredicable t) => t -> String -> Test
pred ~? msg = TestCase (pred @? msg)

(~=?) :: (Eq a, Show a) => a -> a -> Test
expected ~=? actual = TestCase (expected @=? actual)

(~?=) :: (Eq a, Show a) => a -> a -> Test
actual ~?= expected = TestCase (actual @?= expected)

(~:) :: (Testable t) => String -> t -> Test
label ~: t = TestLabel label (test t)
```

`(~?)`, `(~=?)`, and `(~?=)` each make an assertion, as for
 `(@?)`, `(@=?)`, and `(@?=)`, respectively, and then a test case
 from that assertion. `(~:)` attaches a label to something that is
 `Testable`. You may define additional instances for the type class
 `Testable` should that be useful.

## Running Tests

HUnit is structured to support multiple test controllers. The first
 subsection below describes the [test execution](#test-execution)
 characteristics common to all test controllers. The second subsection
 describes the text-based controller that is included with HUnit.

## Test Execution

All test controllers share a common test execution model. They differ only in how the
 results of test execution are shown.

The execution of a test (a value of type `Test`) involves the serial execution (in
 the `IO` monad) of its constituent test cases. The test cases are executed in a
 depth-first, left-to-right order. During test execution, four counts of test cases are
 maintained:

```haskell
data Counts = Counts { cases, tried, errors, failures :: Int }
  deriving (Eq, Show, Read)
```


* `cases` is the number of test cases included in the test. This number is a
    static property of a test and remains unchanged during test execution.
* `tried` is the number of test cases that have been executed so far during the
    test execution.
* `errors` is the number of test cases whose execution ended with an unexpected
    exception being raised. Errors indicate problems with test cases, as opposed to the
    code under test.
* `failures` is the number of test cases whose execution asserted failure.
    Failures indicate problems with the code under test.


Why is there no count for test case successes? The technical reason is that the counts
 are maintained such that the number of test case successes is always equal to
     `(tried - (errors + failures))`. The
 psychosocial reason is that, with test-centered development and the expectation that
 test failures will be few and short-lived, attention should be focused on the failures
 rather than the successes.

As test execution proceeds, three kinds of reporting event are communicated to the test
 controller. (What the controller does in response to the reporting events depends on the
 controller.)

* *start* -- Just prior to initiation of a test case, the path of the test case
    and the current counts (excluding the current test case) are reported.
* *error* -- When a test case terminates with an error, the error message is
    reported, along with the test case path and current counts (including the current
    test case).
* *failure* -- When a test case terminates with a failure, the failure message is
    reported, along with the test case path and current counts (including the current
    test case).

Typically, a test controller shows *error* and *failure* reports immediately
 but uses the *start* report merely to update an indication of overall test
 execution progress.

### Text-Based Controller

A text-based test controller is included with HUnit.

```haskell
runTestText :: PutText st -> Test -> IO (Counts, st)
```

`runTestText` is generalized on a *reporting scheme* given as its first
 argument. During execution of the test given as its second argument, the controller
 creates a string for each reporting event and processes it according to the reporting
 scheme. When test execution is complete, the controller returns the final counts along
 with the final state for the reporting scheme.

The strings for the three kinds of reporting event are as follows.

* A *start* report is the result of the function `showCounts` applied to
    the counts current immediately prior to initiation of the test case being started.
* An *error* report is of the form
            "`Error in:   *path*\n*message*`",
    where *path* is the path of the test case in error, as shown by
    `showPath`, and *message* is a message describing the error. If the path
    is empty, the report has the form "`Error:\n*message*`".
* A *failure* report is of the form
            "`Failure in: *path*\n*message*`", where
        *path* is the path of the test case in error, as shown by
    `showPath`, and *message* is the failure message. If the path is empty,
    the report has the form "`Failure:\n*message*`".

The function `showCounts` shows a set of counts.

```haskell
showCounts :: Counts -> String
```

The form of its result is
`Cases: *cases*  Tried: *tried*  Errors: *errors*  Failures: *failures*`
where *cases*, *tried*, *errors*, and *failures* are the count values.

The function `showPath` shows a test case path.

```haskell
 showPath :: Path -> String
```

The nodes in the path are reversed (so that the path reads from the root down to the test
 case), and the representations for the nodes are joined by '`:`' separators. The
 representation for `(ListItem *n*)` is `(show n)`. The representation
 for `(Label *label*)` is normally *label*. However, if *label*
 contains a colon or if `(show *label*)` is different from *label*
 surrounded by quotation marks--that is, if any ambiguity could exist--then `(Label
         *label*)` is represented as `(show *label*)`.

HUnit includes two reporting schemes for the text-based test controller. You may define
 others if you wish.

```haskell
putTextToHandle :: Handle -> Bool -> PutText Int
```

`putTextToHandle` writes error and failure reports, plus a report of the final
 counts, to the given handle. Each of these reports is terminated by a newline. In
 addition, if the given flag is `True`, it writes start reports to the handle as
 well. A start report, however, is not terminated by a newline. Before the next report is
 written, the start report is "erased" with an appropriate sequence of carriage return
 and space characters. Such overwriting realizes its intended effect on terminal devices.

```haskell
putTextToShowS :: PutText ShowS
```

`putTextToShowS` ignores start reports and simply accumulates error and failure
 reports, terminating them with newlines. The accumulated reports are returned (as the
 second element of the pair returned by `runTestText`) as a `ShowS`
 function (that is, one with type `(String -> String)`) whose
 first argument is a string to be appended to the accumulated report lines.

HUnit provides a shorthand for the most common use of the text-based test controller.

```haskell
runTestTT :: Test -> IO Counts
```

`runTestTT` invokes `runTestText`, specifying `(putTextToHandle stderr
True)` for the reporting scheme, and returns the final counts from the
test execution.

## References

* [1] Gamma, E., et al. Design Patterns: Elements of Reusable Object-Oriented Software, Addison-Wesley, Reading, MA, 1995: The classic book describing design patterns in an object-oriented context.

* [junit.org](http://www.junit.org): Web page for JUnit, the tool after which HUnit is modeled.

* [http://junit.sourceforge.net/doc/testinfected/testing.htm](http://junit.sourceforge.net/doc/testinfected/testing.htm): A good introduction to test-first development and the use of JUnit.

* [http://junit.sourceforge.net/doc/cookstour/cookstour.htm](http://junit.sourceforge.net/doc/cookstour/cookstour.htm): A description of the internal structure of JUnit. Makes for an interesting comparison between JUnit and HUnit.

The HUnit software and this guide were written by Dean Herington [heringto@cs.unc.edu](mailto:heringto@cs.unc.edu)
