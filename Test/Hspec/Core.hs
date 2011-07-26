{-# OPTIONS -XFlexibleInstances -XExistentialQuantification #-}

-- | This module contains the core types, constructors, classes,
-- instances, and utility functions common to hspec.
--
module Test.Hspec.Core where

import System.IO
import System.IO.Silently
import Control.Exception

-- | The result of running an example.
data Result = Success | Pending String | Fail String
  deriving Eq


-- | Everything needed to specify and show a specific behavior.
data Spec = Spec {
                 -- | What is being tested, usually the name of a type.
                 name::String,
                 -- | The specific behavior being tested.
                 requirement::String,
                 -- | The status of the example of this behavior.
                 result::Result }
          | UnevaluatedSpec {
                 -- | What is being tested, usually the name of a type.
                 name::String,
                 -- | The specific behavior being tested.
                 requirement::String,
                 -- | An example of this behavior.
                 example::AnyExample }


-- | An existentialy quantified @Example@
data AnyExample = forall a. Example a => AnyExample a


data Formatter = Formatter { formatterName   :: String,
                             exampleGroupStarted :: Handle -> Spec -> IO (),
                             examplePassed   :: Handle -> Spec -> [String] -> IO (),
                             exampleFailed   :: Handle -> Spec -> [String] -> IO (),
                             examplePending  :: Handle -> Spec -> [String] -> IO (),
                             errorsFormatter :: Handle -> [String] -> IO (),
                             footerFormatter :: Handle -> [Spec] -> Double -> IO (),
                             usesFormatting  :: Bool }


describe :: String -- ^ The name of what is being described, usually a function or type.
         -> [String -> Spec] -- ^ A list of behaviors and examples, created by a list of 'it'.
         -> [Spec]
describe name xs = map ($name) xs


-- | Combine a list of descriptions.
descriptions :: [[Spec]] -> [Spec]
descriptions = concat

-- | Evaluate a Result. Any exceptions (undefined, etc.) are treated as failures.
safely :: Result -> IO Result
safely f = Control.Exception.catch ok failed
  where ok = silence $ f `seq` return f
        failed e = return $ Fail (show (e :: SomeException))


evaluateSpec :: Spec -> IO Spec
evaluateSpec (UnevaluatedSpec name description example) = do
  result <- evaluateExample example
  return $ Spec name description result
evaluateSpec spec = return spec


-- | Create a set of specifications for a specific type being described.
-- Once you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
--
it :: Example a => String -> a -> String -> Spec
it description example name = UnevaluatedSpec name description (AnyExample example)

class Example a where
  evaluateExample :: a -> IO Result

instance Example AnyExample where
  evaluateExample (AnyExample a) = evaluateExample a

instance Example Bool where
  evaluateExample example = safely $ if example then Success else Fail ""

instance Example Result where
  evaluateExample result = safely result


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


failedCount :: [Spec] -> Int
failedCount ss = length $ filter (isFailure.result) ss

failure :: [Spec] -> Bool
failure = any (isFailure.result)

success :: [Spec] -> Bool
success = not . failure

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a more readable display of a quantity of something.
quantify :: Num a => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
