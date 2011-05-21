{-# OPTIONS -XFlexibleInstances #-}

-- | This module contains the core types, constructors, classes,
-- instances, and utility functions common to hspec.
--
module Test.Hspec.Core where

import System.IO
import System.IO.Silently
import Control.Exception
import Control.Monad (liftM)

-- | The result of running an example.
data Result = Success | Fail String | Pending String
  deriving Eq

-- | Everything needed to specify and show a specific behavior.
data Spec = Spec {
                 -- | What is being tested, usually the name of a type.
                 name::String,
                 -- | The specific behavior being tested.
                 requirement::String,
                 -- | The status of this behavior.
                 result::Result }

data Formatter = Formatter { exampleGroupStarted :: Handle -> Spec -> IO (),
                             examplePassed   :: Handle -> Spec -> [String] -> IO (),
                             exampleFailed   :: Handle -> Spec -> [String] -> IO (),
                             examplePending  :: Handle -> Spec -> [String] -> IO (),
                             errorsFormatter :: Handle -> [String] -> IO (),
                             footerFormatter :: Handle -> [Spec] -> Double -> IO () }


-- | Create a set of specifications for a specific type being described.
-- Once you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
--
describe :: String                -- ^ The name of what is being described, usually a function or type.
         -> [IO (String, Result)] -- ^ A list of behaviors and examples, created by a list of 'it'.
         -> IO [IO Spec]
describe n = return . map (>>= \ (req, res) -> return (Spec n req res))

-- | Combine a list of descriptions.
descriptions :: [IO [IO Spec]] -> IO [IO Spec]
descriptions = liftM concat . sequence

-- | Evaluate a Result. Any exceptions (undefined, etc.) are treated as failures.
safely :: Result -> IO Result
safely f = Control.Exception.catch ok failed
  where ok = silence $ f `seq` return f
        failed e = return $ Fail (show (e :: SomeException))

-- | Anything that can be used as an example of a behavior.
class SpecVerifier a where
  -- | Create a description and example of a behavior, a list of these
  -- is used by 'describe'. Once you know what you want to specify, use this.
  --
  -- > describe "closeEnough" [
  -- >   it "is true if two numbers are almost the same"
  -- >     (1.001 `closeEnough` 1.002),
  -- >
  -- >   it "is false if two numbers are not almost the same"
  -- >     (not $ 1.001 `closeEnough` 1.003)
  -- >   ]
  --
  it :: String           -- ^ A description of this behavior.
     -> a                -- ^ An example for this behavior.
     -> IO (String, Result)

instance SpecVerifier Bool where
  it description example = do
    r <- safely (if example then Success else Fail "")
    return (description, r)

instance SpecVerifier Result where
  it description example = do
    r <- safely example
    return (description, r)

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

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a more readable display of a quantity of something.
quantify :: Num a => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
