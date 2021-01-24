-- | Text-based test controller for running HUnit tests and reporting
--   results as text, usually to a terminal.

module Test.HUnit.Text
(
  PutText(..),
  putTextToHandle, putTextToShowS,
  runTestText,
  showPath, showCounts,
  runTestTT,
  runTestTTAndExit
)
where

import Test.HUnit.Base

import Data.CallStack
import Control.Monad (when)
import System.IO (Handle, stderr, hPutStr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)


-- | As the general text-based test controller ('runTestText') executes a
--   test, it reports each test case start, error, and failure by
--   constructing a string and passing it to the function embodied in a
--   'PutText'.  A report string is known as a \"line\", although it includes
--   no line terminator; the function in a 'PutText' is responsible for
--   terminating lines appropriately.  Besides the line, the function
--   receives a flag indicating the intended \"persistence\" of the line:
--   'True' indicates that the line should be part of the final overall
--   report; 'False' indicates that the line merely indicates progress of
--   the test execution.  Each progress line shows the current values of
--   the cumulative test execution counts; a final, persistent line shows
--   the final count values.
--
--   The 'PutText' function is also passed, and returns, an arbitrary state
--   value (called 'st' here).  The initial state value is given in the
--   'PutText'; the final value is returned by 'runTestText'.

data PutText st = PutText (String -> Bool -> st -> IO st) st


-- | Two reporting schemes are defined here.  @putTextToHandle@ writes
-- report lines to a given handle.  'putTextToShowS' accumulates
-- persistent lines for return as a whole by 'runTestText'.
--
-- @putTextToHandle@ writes persistent lines to the given handle,
-- following each by a newline character.  In addition, if the given flag
-- is @True@, it writes progress lines to the handle as well.  A progress
-- line is written with no line termination, so that it can be
-- overwritten by the next report line.  As overwriting involves writing
-- carriage return and blank characters, its proper effect is usually
-- only obtained on terminal devices.

putTextToHandle
    :: Handle
    -> Bool -- ^ Write progress lines to handle?
    -> PutText Int
putTextToHandle handle showProgress = PutText put initCnt
 where
  initCnt = if showProgress then 0 else -1
  put line pers (-1) = do when pers (hPutStrLn handle line); return (-1)
  put line True  cnt = do hPutStrLn handle (erase cnt ++ line); return 0
  put line False _   = do hPutStr handle ('\r' : line); return (length line)
    -- The "erasing" strategy with a single '\r' relies on the fact that the
    -- lengths of successive summary lines are monotonically nondecreasing.
  erase cnt = if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"


-- | Accumulates persistent lines (dropping progess lines) for return by
--   'runTestText'.  The accumulated lines are represented by a
--   @'ShowS' ('String' -> 'String')@ function whose first argument is the
--   string to be appended to the accumulated report lines.

putTextToShowS :: PutText ShowS
putTextToShowS = PutText put id
 where put line pers f = return (if pers then acc f line else f)
       acc f line rest = f (line ++ '\n' : rest)


-- | Executes a test, processing each report line according to the given
--   reporting scheme.  The reporting scheme's state is threaded through calls
--   to the reporting scheme's function and finally returned, along with final
--   count values.

runTestText :: PutText st -> Test -> IO (Counts, st)
runTestText (PutText put us0) t = do
  (counts', us1) <- performTest reportStart reportError reportFailure us0 t
  us2 <- put (showCounts counts') True us1
  return (counts', us2)
 where
  reportStart ss us = put (showCounts (counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 loc msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ "\n" ++ formatLocation loc ++ msg
         kind  = if null path' then p0 else p1
         path' = showPath (path ss)

formatLocation :: Maybe SrcLoc -> String
formatLocation Nothing = ""
formatLocation (Just loc) = srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ "\n"

-- | Converts test execution counts to a string.

showCounts :: Counts -> String
showCounts Counts{ cases = cases', tried = tried',
                   errors = errors', failures = failures' } =
  "Cases: " ++ show cases' ++ "  Tried: " ++ show tried' ++
  "  Errors: " ++ show errors' ++ "  Failures: " ++ show failures'


-- | Converts a test case path to a string, separating adjacent elements by
--   the colon (\':\'). An element of the path is quoted (as with 'show') when
--   there is potential ambiguity.

showPath :: Path -> String
showPath [] = ""
showPath nodes = foldl1 f (map showNode nodes)
 where f b a = a ++ ":" ++ b
       showNode (ListItem n) = show n
       showNode (Label label) = safe label (show label)
       safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s


-- | Provides the \"standard\" text-based test controller. Reporting is made to
--   standard error, and progress reports are included. For possible
--   programmatic use, the final counts are returned.
--
--   The \"TT\" in the name suggests \"Text-based reporting to the Terminal\".

runTestTT :: Test -> IO Counts
runTestTT t = do (counts', 0) <- runTestText (putTextToHandle stderr True) t
                 return counts'

-- | Convenience wrapper for 'runTestTT'.
--   Simply runs 'runTestTT' and then exits back to the OS,
--   using 'exitSuccess' if there were no errors or failures,
--   or 'exitFailure' if there were. For example:
--
--   > tests :: Test
--   > tests = ...
--   >
--   > main :: IO ()
--   > main = runTestTTAndExit tests

runTestTTAndExit :: Test -> IO ()
runTestTTAndExit tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure
