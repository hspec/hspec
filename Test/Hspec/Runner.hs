
-- | This module contains the runners that take a set of specs, evaluate their examples, and
-- report to a given handle.
--
module Test.Hspec.Runner (
  Specs, hspec, hspecX, hspecB, hHspec, hHspecWithFormat, describe, it, toExitCode
) where

import Test.Hspec.Core
import Test.Hspec.Formatters
import Test.Hspec.Formatters.Internal
import System.IO
import System.CPUTime (getCPUTime)
import System.Exit

type Specs = [UnevaluatedSpec]

-- | Evaluate and print the result of checking the spec examples.
runFormatter :: Formatter -> Int -> String -> UnevaluatedSpec -> FormatM EvaluatedSpec
runFormatter formatter nesting _ (SpecGroup group xs) = do
  exampleGroupStarted formatter (nesting) group
  ys <- mapM (runFormatter formatter (succ nesting) group) xs
  return (SpecGroup group ys)
runFormatter formatter nesting group (SpecExample requirement e) = do
  result <- liftIO $ safeEvaluateExample e
  case result of
    Success -> do
      increaseSuccessCount
      exampleSucceeded formatter nesting requirement
    Fail err -> do
      increaseFailCount
      exampleFailed  formatter nesting requirement err
      n <- getFailCount
      addFailMessage $ failureDetails group requirement err n
    Pending reason -> do
      increasePendingCount
      examplePending formatter nesting requirement reason
  return (SpecExample requirement result)

failureDetails :: String -> String -> String -> Int -> String
failureDetails group requirement err i =
  concat [ show i, ") ", group, " ",  requirement, " FAILED", if null err then "" else "\n" ++ err ]

-- | Use in place of @hspec@ to also exit the program with an @ExitCode@
hspecX :: Specs -> IO a
hspecX ss = hspecB ss >>= exitWith . toExitCode

-- | Use in place of hspec to also give a @Bool@ success indication
hspecB :: Specs -> IO Bool
hspecB ss = hspec ss >>= return . success

-- | Create a document of the given specs and write it to stdout.
hspec :: Specs -> IO [EvaluatedSpec]
hspec ss = hHspec stdout ss

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle -> Specs -> IO [EvaluatedSpec]
hHspec h specs = do
  useColor <- hIsTerminalDevice h
  hHspecWithFormat specdoc useColor h specs

-- | Create a document of the given specs and write it to the given handle.
-- THIS IS LIKELY TO CHANGE
hHspecWithFormat :: Formatter -> Bool -> Handle -> Specs -> IO [EvaluatedSpec]
hHspecWithFormat formatter useColor h ss =
           (runFormatM useColor h $ do
         t0 <- liftIO $ getCPUTime
         specList <- mapM (runFormatter formatter 0 "") ss
         t1 <- liftIO $ getCPUTime
         let runTime = ((fromIntegral $ t1 - t0) / (10.0^(12::Integer)) :: Double)
         failedFormatter formatter
         (footerFormatter formatter) runTime
         return specList)

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

