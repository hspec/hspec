module Test.Hspec.Internal (
  Spec (..)
, Example (..)
, safeEvaluateExample
, Result (..)

, describe
, it

, quantify
)
where

import           Control.Exception

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show)

-- IMPORTANT: Remove this warnings and remove -fno-warn-deprecations from the
-- following modules/files when making SpecGroup abstract.
--
--  * Spec.hs
--  * Test.Hspec.Runner
--  * Test.Hspec.Core
--
{-# WARNING SpecExample "This will be removed from the public interface with the next major release.  If you need this, raise your voice!" #-}
{-# WARNING SpecGroup "This will be removed from the public interface with the next major release.  If you need this, raise your voice!" #-}

-- | Internal representation of a spec.
--
-- This will be made abstract with the next release.  If you still need access
-- to any constructors, open an issue and describe your use case:
-- <https://github.com/hspec/hspec/issues>
data Spec = SpecGroup String [Spec]
          | SpecExample String (IO Result)

describe :: String -> [Spec] -> Spec
describe = SpecGroup

safeEvaluateExample :: IO Result -> IO Result
safeEvaluateExample action = do
  action `catches` [
    -- Re-throw AsyncException, otherwise execution will not terminate on
    -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
    -- UserInterrupt) because all of them indicate severe conditions and
    -- should not occur during normal test runs.
    Handler (\e -> throw (e :: AsyncException)),

    Handler (\e -> return $ Fail (show (e :: SomeException)))
    ]


-- | Create a set of specifications for a specific type being described.
-- Once you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
--
it :: Example a => String -> a -> Spec
it requirement = SpecExample requirement . evaluateExample

-- | A type class for examples.
--
-- To use an HUnit `Test.HUnit.Test` or an `Test.HUnit.Assertion` as an example
-- you need to import "Test.Hspec.HUnit".
--
-- To use a QuickCheck `Test.QuickCheck.Property` as an example you need to
-- import "Test.Hspec.QuickCheck".
class Example a where
  evaluateExample :: a -> IO Result

instance Example Bool where
  evaluateExample b = if b then return Success else return (Fail "")

instance Example Result where
  evaluateExample r = r `seq` return r

-- | Create a more readable display of a quantity of something.
--
-- Examples:
--
-- >>> quantify 0 "example"
-- "0 examples"
--
-- >>> quantify 1 "example"
-- "1 example"
--
-- >>> quantify 2 "example"
-- "2 examples"
quantify :: Int -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
