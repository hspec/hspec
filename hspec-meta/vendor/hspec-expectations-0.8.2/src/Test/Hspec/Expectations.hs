{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations (

-- * Setting expectations
  Expectation
, expectationFailure
, shouldBe
, shouldSatisfy
, shouldStartWith
, shouldEndWith
, shouldContain
, shouldMatchList
, shouldReturn

, shouldNotBe
, shouldNotSatisfy
, shouldNotContain
, shouldNotReturn

-- * Expecting exceptions
, shouldThrow

-- ** Selecting exceptions
, Selector

-- ** Predefined type-based selectors
-- |
-- There are predefined selectors for some standard exceptions.  Each selector
-- is just @const True@ with an appropriate type.
, anyException
, anyErrorCall
, anyIOException
, anyArithException

-- ** Combinators for defining value-based selectors
-- |
-- Some exceptions (most prominently `ErrorCall`) have no `Eq` instance.
-- Selecting a specific value would require pattern matching.
--
-- For such exceptions, combinators that construct selectors are provided.
-- Each combinator corresponds to a constructor; it takes the same arguments,
-- and has the same name (but starting with a lower-case letter).
, errorCall

-- * Re-exports
, HasCallStack
) where

import qualified Test.HUnit
import           Test.HUnit ((@?=))
import           Control.Exception
import           Data.Typeable
import           Data.List

import           Control.Monad (unless)

import           Test.Hspec.Expectations.Matcher

#if MIN_VERSION_HUnit(1,4,0)
import           Data.CallStack (HasCallStack)
#else
#if MIN_VERSION_base(4,8,1)
import qualified GHC.Stack as GHC
type HasCallStack = (?loc :: GHC.CallStack)
#else
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
#endif
#endif

type Expectation = Test.HUnit.Assertion

expectationFailure :: HasCallStack => String -> Expectation
expectationFailure = Test.HUnit.assertFailure

expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

infix 1 `shouldBe`, `shouldSatisfy`, `shouldStartWith`, `shouldEndWith`, `shouldContain`, `shouldMatchList`, `shouldReturn`, `shouldThrow`
infix 1 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
actual `shouldBe` expected = actual @?= expected

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation
v `shouldSatisfy` p = expectTrue ("predicate failed on: " ++ show v) (p v)

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

-- |
-- @list \`shouldStartWith\` prefix@ sets the expectation that @list@ starts with @prefix@,
shouldStartWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldStartWith = compareWith isPrefixOf "does not start with"

-- |
-- @list \`shouldEndWith\` suffix@ sets the expectation that @list@ ends with @suffix@,
shouldEndWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldEndWith = compareWith isSuffixOf "does not end with"

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in @list@.
shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldContain = compareWith isInfixOf "does not contain"

-- |
-- @xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same
-- elements that @ys@ has, possibly in another order
shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
xs `shouldMatchList` ys = maybe (return ()) expectationFailure (matchList xs ys)

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
action `shouldReturn` expected = action >>= (`shouldBe` expected)

-- |
-- @actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not
-- equal to @notExpected@
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
actual `shouldNotBe` notExpected = expectTrue ("not expected: " ++ show actual) (actual /= notExpected)

-- |
-- @v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation
v `shouldNotSatisfy` p = expectTrue ("predicate succeeded on: " ++ show v) ((not . p) v)

-- |
-- @list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not
-- contained anywhere in @list@.
shouldNotContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
list `shouldNotContain` sublist = expectTrue errorMsg ((not . isInfixOf sublist) list)
  where
    errorMsg = show list ++ " does contain " ++ show sublist

-- |
-- @action \`shouldNotReturn\` notExpected@ sets the expectation that @action@
-- does not return @notExpected@.
shouldNotReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)

-- |
-- A @Selector@ is a predicate; it can simultaneously constrain the type and
-- value of an exception.
type Selector a = (a -> Bool)

-- |
-- @action \`shouldThrow\` selector@ sets the expectation that @action@ throws
-- an exception.  The precise nature of the expected exception is described
-- with a 'Selector'.
shouldThrow :: (HasCallStack, Exception e) => IO a -> Selector e -> Expectation
action `shouldThrow` p = do
  r <- try action
  case r of
    Right _ ->
      expectationFailure $
        "did not get expected exception: " ++ exceptionType
    Left e ->
      (`expectTrue` p e) $
        "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

anyException :: Selector SomeException
anyException = const True

anyErrorCall :: Selector ErrorCall
anyErrorCall = const True

errorCall :: String -> Selector ErrorCall
#if MIN_VERSION_base(4,9,0)
errorCall s (ErrorCallWithLocation msg _) = s == msg
#else
errorCall s (ErrorCall msg) = s == msg
#endif

anyIOException :: Selector IOException
anyIOException = const True

anyArithException :: Selector ArithException
anyArithException = const True
