{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#define HasCallStack_ HasCallStack =>
#else
#define HasCallStack_
#endif

module Test.HUnit.Lang (
  Assertion,
  assertFailure,
  assertEqual,

  Result (..),
  performTestCase,
-- * Internals
-- |
-- /Note:/ This is not part of the public API!  It is exposed so that you can
-- tinker with the internals of HUnit, but do not expect it to be stable!
  HUnitFailure (..),
  FailureReason (..),
  formatFailureReason
) where

import           Control.DeepSeq
import           Control.Exception as E
import           Control.Monad
import           Data.List
import           Data.Typeable
import           Data.CallStack

-- | When an assertion is evaluated, it will output a message if and only if the
-- assertion fails.
--
-- Test cases are composed of a sequence of one or more assertions.
type Assertion = IO ()

data HUnitFailure = HUnitFailure (Maybe SrcLoc) FailureReason
    deriving (Eq, Show, Typeable)

instance Exception HUnitFailure

data FailureReason = Reason String | ExpectedButGot (Maybe String) String String
    deriving (Eq, Show, Typeable)

location :: HasCallStack_ Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- | Unconditionally signals that a failure has occurred.
assertFailure ::
     HasCallStack_
     String -- ^ A message that is displayed with the assertion failure
  -> IO a
assertFailure msg = msg `deepseq` E.throwIO (HUnitFailure location $ Reason msg)

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual :: HasCallStack_ (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqual preface expected actual =
  unless (actual == expected) $ do
    (prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    expectedMsg = show expected
    actualMsg = show actual

formatFailureReason :: FailureReason -> String
formatFailureReason (Reason reason) = reason
formatFailureReason (ExpectedButGot preface expected actual) = intercalate "\n" . maybe id (:) preface $ ["expected: " ++ expected, " but got: " ++ actual]

data Result = Success | Failure (Maybe SrcLoc) String | Error (Maybe SrcLoc) String
  deriving (Eq, Show)

-- | Performs a single test case.
performTestCase :: Assertion -- ^ an assertion to be made during the test case run
                -> IO Result
performTestCase action =
  (action >> return Success)
     `E.catches`
      [E.Handler (\(HUnitFailure loc reason) -> return $ Failure loc (formatFailureReason reason)),

       -- Re-throw AsyncException, otherwise execution will not terminate on
       -- SIGINT (ctrl-c).  Currently, all AsyncExceptions are being thrown
       -- because it's thought that none of them will be encountered during
       -- normal HUnit operation.  If you encounter an example where this
       -- is not the case, please email the maintainer.
       E.Handler (\e -> throw (e :: E.AsyncException)),

       E.Handler (\e -> return $ Error Nothing $ show (e :: E.SomeException))]
