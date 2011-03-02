{-# OPTIONS -XFlexibleInstances #-}

module Test.Hspec.Internal where

import System.IO
import System.Exit
import Data.List (mapAccumL, groupBy, intersperse)
import System.CPUTime (getCPUTime)
import Text.Printf
import Control.Exception

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
         -> IO [Spec]
describe n ss = do
  ss' <- sequence ss
  return $ map (\ (req, res) -> Spec n req res) ss'


-- | Evaluate a Result. Any exceptions (undefined, etc.) are treated as failures.
safely :: Result -> IO Result
safely f = Control.Exception.catch ok failed
  where ok = f `seq` return f
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
  it description example = return (description, example)


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


-- | Create a document of the given specs.
documentSpecs :: [Spec] -> [String]
documentSpecs specs = lines $ unlines $ concat report ++ [""] ++ intersperse "" errors
  where organize = groupBy (\ a b -> name a == name b)
        (errors, report) = mapAccumL documentGroup [] $ organize specs

-- | Create a document of the given group of specs.
documentGroup :: [String] -> [Spec] -> ([String], [String])
documentGroup errors specGroup = (errors', "" : name (head specGroup) : report)
  where (errors', report) = mapAccumL documentSpec errors specGroup

-- | Create a document of the given spec.
documentSpec :: [String] -> Spec -> ([String], String)
documentSpec errors spec = case result spec of
                Success    -> (errors, " - " ++ requirement spec)
                Fail s     -> (errors ++ [errorDetails s], " x " ++ requirement spec ++ errorTag)
                Pending s  -> (errors, " - " ++ requirement spec ++ "\n     # " ++ s)
    where errorTag = " [" ++ (show $ length errors + 1) ++ "]"
          errorDetails s  = concat [ show (length errors + 1), ") ", name spec,
                                     " ",  requirement spec, " FAILED",
                                     if null s then "" else "\n" ++ s ]


-- | Create a summary of how long it took to run the examples.
timingSummary :: Double -> String
timingSummary t = printf "Finished in %1.4f seconds" (t / (10.0^(12::Integer)) :: Double)

failedCount :: [Spec] -> Int
failedCount ss = length $ filter (isFailure.result) ss
  where
    isFailure (Fail _) = True
    isFailure _        = False

-- | Create a summary of how many specs exist and how many examples failed.
successSummary :: [Spec] -> String
successSummary ss = quantify (length ss) "example" ++ ", " ++ quantify (failedCount ss) "failure"


-- | Create a document of the given specs.
-- This does not track how much time it took to check the examples. If you want
-- a description of each spec and don't need to know how long it tacks to check,
-- use this.
pureHspec :: [Spec]   -- ^ The specs you are interested in.
          -> [String]
pureHspec = fst . pureHspecB


pureHspecB :: [Spec]   -- ^ The specs you are interested in.
          -> ([String], Bool)
pureHspecB ss = (report, failedCount ss == 0)
  where report = documentSpecs ss ++ [ "", timingSummary 0, "", successSummary ss]


-- | Create a document of the given specs and write it to stdout.
-- This does track how much time it took to check the examples. Use this if
-- you want a description of each spec and do need to know how long it tacks
-- to check the examples or want to write to stdout.
hspec :: IO [Spec] -> IO ()
hspec ss = hspecB ss >> return ()

-- | Same as 'hspec' except it returns a bool indicating if all examples ran without failures
hspecB :: IO [Spec] -> IO Bool
hspecB = hHspec stdout

-- | Same as 'hspec' except the program exits successfull if all examples ran without failures or
-- with an errorcode of 1 if any examples failed.
hspecX :: IO [Spec] -> IO a
hspecX ss = hspecB ss >>= exitWith . toExitCode

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1


-- | Create a document of the given specs and write it to the given handle.
-- This does track how much time it took to check the examples. Use this if
-- you want a description of each spec and do need to know how long it tacks
-- to check the examples or want to write to a file or other handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle     -- ^ A handle for the stream you want to write to.
       -> IO [Spec]  -- ^ The specs you are interested in.
       -> IO Bool
hHspec h ss = do
  t0 <- getCPUTime
  ss' <- ss
  mapM_ (hPutStrLn h) $ documentSpecs ss'
  t1 <- getCPUTime
  mapM_ (hPutStrLn h) [ "", timingSummary (fromIntegral $ t1 - t0), "", successSummary ss']
  return $ failedCount ss' == 0


-- | Create a more readable display of a quantity of something.
quantify :: Num a => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
