-- |
-- This module contains the core types, constructors, classes, instances, and
-- utility functions common to hspec.
module Test.Hspec.Core where

import           System.IO.Silently
import           Control.Exception

-- | The result of running an example.
data Result = Success | Pending String | Fail String
  deriving (Eq, Show)


type UnevaluatedSpec = Spec (IO Result)
type EvaluatedSpec = Spec Result

data Spec a = SpecGroup String [Spec a]
            | SpecExample String a

describe :: String -> [Spec a] -> Spec a
describe = SpecGroup

-- | DEPRECATED: This is no longer needed (it's just an alias for `id` now).
descriptions :: [Spec a] -> [Spec a]
descriptions = id
{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}

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
it :: Example a => String -> a -> UnevaluatedSpec
it requirement' = SpecExample requirement' . evaluateExample

class Example a where
  evaluateExample :: a -> IO Result

instance Example Bool where
  evaluateExample bool = evaluateExample $ if bool then Success else Fail ""

instance Example Result where
  evaluateExample result' = silence $ result' `seq` return result'

-- | Declare an example as not successful or failing but pending some other work.
-- If you want to report on a behavior but don't have an example yet, use this.
--
-- > describe "fancyFormatter" [
-- >   it "can format text in a way that everyone likes"
-- >     (pending "waiting for clarification from the designers")
-- >   ]
--
pending :: String  -- ^ An explanation for why this behavior is pending.
        -> Result
pending = Pending


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
