{-# OPTIONS -XFlexibleInstances #-}

module Test.Hspec.Internal where

import System.IO
import System.IO.Silently
import Data.List (intersperse)
import System.CPUTime (getCPUTime)
import Text.Printf
import Control.Exception
import Control.Monad (liftM, when)
import System.Console.ANSI

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

specdoc :: Bool -> Formatter
specdoc useColor = Formatter {
  exampleGroupStarted = \ h spec -> do
    when useColor (normalColor h)
    hPutStr h ('\n' : name spec ++ "\n"),

  examplePassed = \ h spec _ -> do
    when useColor (passColor h)
    hPutStrLn h $ " - " ++ requirement spec,

  exampleFailed = \ h spec errors -> do
    when useColor (failColor h)
    hPutStrLn h $ " x " ++ requirement spec ++ " FAILED [" ++ (show $ (length errors) + 1) ++ "]",

  examplePending = \ h spec _ -> do
    when useColor (pendingColor h)
    let (Pending s) = result spec
    hPutStrLn h $ " - " ++ requirement spec ++ "\n     # " ++ s,

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ([""] ++ intersperse "" errors),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h ""
    hPutStrLn h $ timingSummary time
    hPutStrLn h ""
    hPutStrLn h $ successSummary specs
    when useColor (normalColor h)
  }

-- | Create a summary of how long it took to run the examples.
timingSummary :: Double -> String
timingSummary t = printf "Finished in %1.4f seconds" (t / (10.0^(12::Integer)) :: Double)

-- | Create a summary of how many specs exist and how many examples failed.
successSummary :: [Spec] -> String
successSummary ss = quantify (length ss) "example" ++ ", " ++ quantify (failedCount ss) "failure"

-- | Evaluate and print the result of checking the spec examples.
runFormatter :: Formatter -> Handle -> String -> [String] -> [IO Spec] -> IO [Spec]
runFormatter formatter h _     errors []     = do
  errorsFormatter formatter h (reverse errors)
  return []
runFormatter formatter h group errors (iospec:ioss) = do
  spec <- iospec
  when (group /= name spec) (exampleGroupStarted formatter h spec)
  case result spec of
    (Success  ) -> examplePassed formatter h spec errors
    (Fail _   ) -> exampleFailed formatter h spec errors
    (Pending _) -> examplePending formatter h spec errors
  let errors' = if isFailure (result spec)
                then errorDetails spec (length errors) : errors
                else errors
  specs <- runFormatter formatter h (name spec) errors' ioss
  return $ specs ++ [spec]

errorDetails :: Spec -> Int -> String
errorDetails spec i = case result spec of
  (Fail s   ) -> concat [ show (i + 1), ") ", name spec, " ",  requirement spec, " FAILED", if null s then "" else "\n" ++ s ]
  _           -> ""

failColor :: Handle -> IO()
failColor h = hSetSGR h [ SetColor Foreground Dull Red ]

passColor :: Handle -> IO()
passColor h = hSetSGR h [ SetColor Foreground Dull Green ]

pendingColor :: Handle -> IO()
pendingColor h = hSetSGR h [ SetColor Foreground Dull Yellow ]

normalColor :: Handle -> IO()
normalColor h = hSetSGR h [ Reset ]

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

-- | Create a document of the given specs and write it to stdout.
hspec :: IO [IO Spec] -> IO [Spec]
hspec ss = hHspec stdout ss

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle     -- ^ A handle for the stream you want to write to.
       -> IO [IO Spec]  -- ^ The specs you are interested in.
       -> IO [Spec]
hHspec h = hHspecWithFormat (specdoc $ h == stdout) h

-- | Create a document of the given specs and write it to the given handle.
-- THIS IS LIKELY TO CHANGE
hHspecWithFormat :: Formatter
                 -> Handle     -- ^ A handle for the stream you want to write to.
                 -> IO [IO Spec]  -- ^ The specs you are interested in.
                 -> IO [Spec]
hHspecWithFormat formatter h ss = do
  t0 <- getCPUTime
  ioSpecList <- ss
  specList <- (runFormatter formatter) h "" [] ioSpecList
  t1 <- getCPUTime
  (footerFormatter formatter) h specList (fromIntegral $ t1 - t0)
  return specList

-- | Create a more readable display of a quantity of something.
quantify :: Num a => a -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
