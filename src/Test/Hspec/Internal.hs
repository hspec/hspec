{-# OPTIONS -XFlexibleInstances #-}

module Test.Hspec.Internal where

import System.IO
import System.IO.Silently
import Data.List (intersperse)
import System.CPUTime (getCPUTime)
import Text.Printf
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



-- | Print the result of checking the spec examples and return the specs.
printSpecReport :: Handle -> [IO Spec] -> IO [Spec]
printSpecReport h = printSpecReport' h "" []

-- Helper function that takes the current group, the errors so far, and the specs.
printSpecReport' :: Handle -> String -> [String] -> [IO Spec] -> IO [Spec]
printSpecReport' h _     errors []     = mapM_ (hPutStrLn h) ([""] ++ intersperse "" errors) >> return []
printSpecReport' h group errors (iospec:ioss) = do
  ~spec@(Spec ~lazyName ~lazyRequirement ~lazyResult) <- iospec
  hPutStr h $ linePrefix group lazyName
  hPutStr h $ lineBody lazyRequirement
  hPutStrLn h $ (lineSuffix lazyResult (length errors))
  let errors' = if isFailure lazyResult
                then errorDetails spec (length errors) : errors
                else errors
  specs <- printSpecReport' h lazyName errors' ioss
  return $ specs ++ [spec]

errorDetails :: Spec -> Int -> String
errorDetails spec i = case result spec of
  (Fail s   ) -> concat [ show (i + 1), ") ", name spec, " ",  requirement spec, " FAILED", if null s then "" else "\n" ++ s ]
  _           -> ""

linePrefix :: String -> String -> String
linePrefix currentGroup group
  | currentGroup /= group = '\n' : group ++ "\n"
  | otherwise             = ""

lineBody :: String -> String
lineBody req = " - " ++ req

lineSuffix :: Result -> Int -> String
lineSuffix (Success  ) _ = ""
lineSuffix (Fail _   ) i = " FAILED [" ++ (show $ i + 1) ++ "]"
lineSuffix (Pending s) _ = "\n     # " ++ s

-- | Create a summary of how long it took to run the examples.
timingSummary :: Double -> String
timingSummary t = printf "Finished in %1.4f seconds" (t / (10.0^(12::Integer)) :: Double)

failedCount :: [Spec] -> Int
failedCount ss = length $ filter (isFailure.result) ss

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

-- | Create a summary of how many specs exist and how many examples failed.
successSummary :: [Spec] -> String
successSummary ss = quantify (length ss) "example" ++ ", " ++ quantify (failedCount ss) "failure"

-- | Create a document of the given specs and write it to stdout.
-- This does track how much time it took to check the examples. Use this if
-- you want a description of each spec and do need to know how long it tacks
-- to check the examples or want to write to stdout.
hspec :: IO [IO Spec] -> IO [Spec]
hspec ss = hHspec stdout ss

-- | Create a document of the given specs and write it to the given handle.
-- This does track how much time it took to check the examples. Use this if
-- you want a description of each spec and do need to know how long it tacks
-- to check the examples or want to write to a file or other handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle     -- ^ A handle for the stream you want to write to.
       -> IO [IO Spec]  -- ^ The specs you are interested in.
       -> IO [Spec]
hHspec h ss = do
  t0 <- getCPUTime
  ss1 <- ss
  ss' <- printSpecReport h ss1
  t1 <- getCPUTime
  mapM_ (hPutStrLn h) [ "", timingSummary (fromIntegral $ t1 - t0), "", successSummary ss']
  return ss'

-- | Create a more readable display of a quantity of something.
quantify :: Num a => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
