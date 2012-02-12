{-# OPTIONS -XFlexibleInstances -XExistentialQuantification #-}

-- | This module contains the core types, constructors, classes,
-- instances, and utility functions common to hspec.
--
module Test.Hspec.Core where

import System.IO.Silently
import Control.Exception

-- | The result of running an example.
data Result = Success | Pending String | Fail String
  deriving Eq


type UnevaluatedSpec = Spec AnyExample
type EvaluatedSpec = Spec Result

-- | Everything needed to specify and show a specific behavior.
data Spec a = Spec {
                 -- | What is being tested, usually the name of a type or use case.
                 name::String,
                 -- | A description of the specific behavior being tested.
                 requirement::String,
                 -- | The status of the example of this behavior.
                 -- | An example of this behavior.
                 -- (either evaluatede or unevaluated)
                 example :: a,
                 -- | The level of nestedness.
                 depth::Int }

describe :: String -> [[Spec a]] -> [Spec a]
describe label specs = map desc (concat specs)
  where desc spec
          | null $ name spec = spec { name = label }
          | otherwise        = spec { depth = depth spec + 1 }

-- | Combine a list of descriptions.
descriptions :: [[Spec a]] -> [Spec a]
descriptions = concat


evaluateSpec :: UnevaluatedSpec -> IO EvaluatedSpec
evaluateSpec (Spec name' requirement' example' depth') = do
  r <- evaluateExample example' `catches` [
    -- Re-throw AsyncException, otherwise execution will not terminate on
    -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
    -- UserInterrupt) because all of them indicate severe conditions and
    -- should not occur during normal test runs.
    Handler (\e -> throw (e :: AsyncException)),

    Handler (\e -> return $ Fail (show (e :: SomeException)))
    ]
  return $ Spec name' requirement' r depth'


-- | Create a set of specifications for a specific type being described.
-- Once you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
--
it :: Example a => String -> a -> [UnevaluatedSpec]
it requirement' example' = [Spec "" requirement' (AnyExample example') 0]

class Example a where
  evaluateExample :: a -> IO Result

instance Example Bool where
  evaluateExample bool = evaluateExample $ if bool then Success else Fail ""

instance Example Result where
  evaluateExample result' = silence $ result' `seq` return result'

-- | An existentially quantified @Example@. This way they can be mixed within the same set of Specs
data AnyExample = forall a. Example a => AnyExample a

instance Example AnyExample where
  evaluateExample (AnyExample a) = evaluateExample a



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
failedCount ss = length $ filter (isFailure . example) ss

failure :: [EvaluatedSpec] -> Bool
failure = any (isFailure . example)

success :: [EvaluatedSpec] -> Bool
success = not . failure

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a more readable display of a quantity of something.
quantify :: (Show a, Num a, Eq a) => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
