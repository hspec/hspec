
-- | This module contains the runners that take a set of specs, evaluate their examples, and
-- report to a given handle.
--
module Test.Hspec.Runner (
  hspec, hspecX, hspecB, hHspec, hHspecWithFormat, describe, it, toExitCode, toIts
) where

import Test.Hspec.Core
import Test.Hspec.Formatters
import System.IO
import System.CPUTime (getCPUTime)
import Control.Monad (when, foldM)
import System.Exit
import Control.Exception (bracket_)

-- convert the tree into the old data structure of unested it blocks
toIts :: [Spec] -> [Spec]
toIts = concatMap (toSpec "")
  where
    toSpec :: String -> Spec-> [Spec]
    toSpec group (ItSpec r spec) = [ItSpec (group ++ r) spec]
    toSpec parentGroup (DescribeSpec group specs) =
      concatMap (toSpec (parentGroup ++ group)) specs

-- | Evaluate and print the result of checking the spec examples.
runFormatter :: Formatter -> Handle -> [Spec] -> IO [Spec]
runFormatter formatter handle specs = do
    (errors, _) <- foldM runFormatter' ([], []) specs
    errorsFormatter formatter handle (reverse errors)
    return specs
  where
    runFormatter' :: ([String], [String]) -> Spec -> IO ([String], [String])
    runFormatter' (errors, descriptions) (DescribeSpec desc its) = do
      (exampleGroupStarted formatter handle desc)
      foldM runFormatter' (errors, descriptions ++ [desc]) its

    runFormatter' (errors, descriptions) it@(ItSpec _ res) = do
      case res of
        (Success) -> examplePassed formatter handle it errors
        (Fail _ ) -> exampleFailed formatter handle it errors
        (Pending _) -> examplePending formatter handle it errors
      return $ (
                 ( if isFailure res
                    then errorDetails descriptions it (length errors) : errors
                    else errors
                 ),
                 descriptions
               )

errorDetails :: [String] -> Spec -> Int -> String
errorDetails descriptions (ItSpec req res) i = case res of
  (Fail s   ) -> concat [ show (i + 1), ") ", concatMap (\d -> d ++ " ") descriptions,  req, " FAILED", if null s then "" else "\n" ++ s ]
  _           -> ""
errorDetails _ _ _ = error "expected an it"

-- | Use in place of @hspec@ to also exit the program with an @ExitCode@
hspecX :: IO [Spec] -> IO a
hspecX ss = hspecB ss >>= exitWith . toExitCode

-- | Use in place of hspec to also give a @Bool@ success indication
hspecB :: IO [Spec] -> IO Bool
hspecB ss = hspec ss >>= return . success

-- | Create a document of the given specs and write it to stdout.
hspec :: IO [Spec] -> IO [Spec]
hspec ss = hHspec stdout ss

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle -> IO [Spec] -> IO [Spec]
hHspec h = hHspecWithFormat (specdoc $ h == stdout) h

-- | Create a document of the given specs and write it to the given handle.
-- THIS IS LIKELY TO CHANGE
hHspecWithFormat :: Formatter -> Handle -> IO [Spec] -> IO [Spec]
hHspecWithFormat formatter h ss =
  bracket_ (when (usesFormatting formatter) $ restoreFormat h)
           (when (usesFormatting formatter) $ restoreFormat h)
           (do
         t0 <- getCPUTime
         ioSpecList <- ss
         specList <- runFormatter formatter h ioSpecList
         t1 <- getCPUTime
         let runTime = ((fromIntegral $ t1 - t0) / (10.0^(12::Integer)) :: Double)
         (footerFormatter formatter) h specList runTime
         return specList)

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

