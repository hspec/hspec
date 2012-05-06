{-# LANGUAGE FlexibleInstances #-}
-- |
-- This module contains the core types, constructors, classes, instances, and
-- utility functions common to hspec.
module Test.Hspec.Internal (
  SpecTree (..)
, Specs
, Spec (..)
, EvaluatedSpec
, Example (..)
, safeEvaluateExample
, Pending
, Result (..)

, describe
, it
, pending

, quantify
, success
, failedCount -- unused, remove?
)
where

import           Control.Exception

-- | The result of running an example.
data Result = Success | ResultPending (Maybe String) | Fail String
  deriving (Eq, Show)


newtype Spec = Spec {unSpec :: SpecTree (IO Result)}

type EvaluatedSpec = SpecTree Result

-- | Internal representation of a spec.
data SpecTree a = SpecGroup String [SpecTree a]
            | SpecExample String a

describe :: String -> [Spec] -> Spec
describe str specs = Spec . SpecGroup str $ map unSpec specs

type Specs = [Spec]

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
it requirement' = Spec . SpecExample requirement' . evaluateExample

class Example a where
  evaluateExample :: a -> IO Result

instance Example Bool where
  evaluateExample b = if b then return Success else return (Fail "")

instance Example Result where
  evaluateExample r = r `seq` return r

newtype Pending = Pending (Maybe String)

-- | A pending example.
--
-- If you want to report on a behavior but don't have an example yet, use this.
--
-- > describe "fancyFormatter" [
-- >   it "can format text in a way that everyone likes" $
-- >     pending
-- > ]
--
-- You can give an optional reason for why it's pending.
--
-- > describe "fancyFormatter" [
-- >   it "can format text in a way that everyone likes" $
-- >     pending "waiting for clarification from the designers"
-- > ]
pending :: String -> Pending
pending = Pending . Just

instance Example Pending where
  evaluateExample (Pending reason) = evaluateExample (ResultPending reason)

instance Example (String -> Pending) where
  evaluateExample _ = evaluateExample (Pending Nothing)


failedCount :: [EvaluatedSpec] -> Int
failedCount = sum . map count
  where
    count (SpecGroup _ xs) = sum (map count xs)
    count (SpecExample _ x) = if isFailure x then 1 else 0

failure :: [EvaluatedSpec] -> Bool
failure = any p
  where
    p (SpecGroup _ xs) = any p xs
    p (SpecExample _ x) = isFailure x

success :: [EvaluatedSpec] -> Bool
success = not . failure

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a more readable display of a quantity of something.
quantify :: (Show a, Num a, Eq a) => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
