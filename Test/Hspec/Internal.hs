{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Internal (
  SpecTree (..)
, Spec (..)
, Example (..)
, safeEvaluateExample
, Result (..)

, describe
, it

, quantify
)
where

import           Control.Exception
import qualified Test.Hspec.Pending as Pending

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show)

newtype Spec = Spec {unSpec :: SpecTree (IO Result)}

-- | Internal representation of a spec.
data SpecTree a = SpecGroup String [SpecTree a]
            | SpecExample String a

describe :: String -> [Spec] -> Spec
describe str specs = Spec . SpecGroup str $ map unSpec specs

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

instance Example Pending.Pending where
  evaluateExample (Pending.Pending reason) = evaluateExample (Pending reason)

instance Example (String -> Pending.Pending) where
  evaluateExample _ = evaluateExample (Pending.Pending Nothing)

-- | Create a more readable display of a quantity of something.
quantify :: (Show a, Num a, Eq a) => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
