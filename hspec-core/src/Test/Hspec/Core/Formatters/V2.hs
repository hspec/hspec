{-# LANGUAGE CPP #-}
-- |
-- Stability: experimental
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Core.Runner.hspecWith`.
module Test.Hspec.Core.Formatters.V2 (

-- * Formatters
  silent
, checks
, specdoc
, progress
, failed_examples

-- * Implementing a custom Formatter
-- |
-- A formatter is a set of actions.  Each action is evaluated when a certain
-- situation is encountered during a test run.
--
-- Actions live in the `FormatM` monad.  It provides access to the runner state
-- and primitives for appending to the generated report.
, Formatter (..)
, Item(..)
, Result(..)
, FailureReason (..)
, FormatM
, formatterToFormat

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount
, getExpectedTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, printTimes

, Seconds(..)
, getCPUTime
, getRealTime

-- ** Appending to the generated report
, write
, writeLine
, writeTransient

-- ** Dealing with colors
, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, outputUnicode

, useDiff
, prettyPrint
, prettyPrintFunction
, extraChunk
, missingChunk

-- ** Helpers
, formatLocation
, formatException

#ifdef TEST
, Chunk(..)
, ColorChunk(..)
, indentChunks
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (First)

import           Data.Char
import           Data.Maybe
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example (Location(..))
import           Test.Hspec.Core.Format(PrettyPrintFunction)
import           Text.Printf
import           Test.Hspec.Core.Formatters.Pretty.Unicode (ushow)
import           Control.Monad.IO.Class
import           Control.Exception
import           Control.Concurrent.Async

-- We use an explicit import list for "Test.Hspec.Formatters.Monad", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.Internal (
    Formatter(..)
  , Item(..)
  , Result(..)
  , FailureReason (..)
  , FormatM
  , formatterToFormat

  , getSuccessCount
  , getPendingCount
  , getFailCount
  , getTotalCount
  , getExpectedTotalCount

  , FailureRecord (..)
  , getFailMessages
  , usedSeed

  , printTimes
  , getCPUTime
  , getRealTime

  , write
  , writeLine
  , writeTransient

  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor

  , outputUnicode

  , useDiff
  , prettyPrint
  , prettyPrintFunction
  , extraChunk
  , missingChunk
  )

import           Test.Hspec.Core.Formatters.Diff

silent :: Formatter
silent = Formatter {
  formatterStarted      = return ()
, formatterGroupStarted = \ _ -> return ()
, formatterGroupDone    = \ _ -> return ()
, formatterProgress     = \ _ _ -> return ()
, formatterItemStarted  = \ _ -> return ()
, formatterItemDone     = \ _ _ -> return ()
, formatterDone         = return ()
}

checks :: Formatter
checks = specdoc {
  formatterProgress = \(nesting, requirement) p -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [" ++ (formatProgress p) ++ "]"

, formatterItemStarted = \(nesting, requirement) -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [ ]"

, formatterItemDone = \ (nesting, requirement) item -> do
    unicode <- outputUnicode
    let fallback a b = if unicode then a else b
    uncurry (writeResult nesting requirement (itemDuration item) (itemInfo item)) $ case itemResult item of
      Success {} -> (withSuccessColor, fallback "✔" "v")
      Pending {} -> (withPendingColor, fallback "‐" "-")
      Failure {} -> (withFailColor,    fallback "✘" "x")
    case itemResult item of
      Success {} -> return ()
      Failure {} -> return ()
      Pending _ reason -> withPendingColor $ do
        writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult :: [String] -> String -> Seconds -> String -> (FormatM () -> FormatM ()) -> String -> FormatM ()
    writeResult nesting requirement duration info withColor symbol = do
      shouldPrintTimes <- printTimes
      write $ indentationFor nesting ++ requirement ++ " ["
      withColor $ write symbol
      writeLine $ "]" ++ if shouldPrintTimes then times else ""
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s
      where
        dt :: Int
        dt = toMilliseconds duration

        times
          | dt == 0 = ""
          | otherwise = " (" ++ show dt ++ "ms)"

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

specdoc :: Formatter
specdoc = silent {

  formatterStarted = do
    writeLine ""

, formatterGroupStarted = \ (nesting, name) -> do
    writeLine (indentationFor nesting ++ name)

, formatterProgress = \_ p -> do
    writeTransient (formatProgress p)

, formatterItemDone = \(nesting, requirement) item -> do
    let duration = itemDuration item
        info = itemInfo item

    case itemResult item of
      Success -> withSuccessColor $ do
        writeResult nesting requirement duration info
      Pending _ reason -> withPendingColor $ do
        writeResult nesting requirement duration info
        writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
      Failure {} -> withFailColor $ do
        n <- getFailCount
        writeResult nesting (requirement ++ " FAILED [" ++ show n ++ "]") duration info

, formatterDone = defaultFailedFormatter >> defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult nesting requirement (Seconds duration) info = do
      shouldPrintTimes <- printTimes
      writeLine $ indentationFor nesting ++ requirement ++ if shouldPrintTimes then times else ""
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s
      where
        dt :: Int
        dt = floor (duration * 1000)

        times
          | dt == 0 = ""
          | otherwise = " (" ++ show dt ++ "ms)"

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

progress :: Formatter
progress = failed_examples {
  formatterItemDone = \ _ item -> case itemResult item of
    Success{} -> withSuccessColor $ write "."
    Pending{} -> withPendingColor $ write "."
    Failure{} -> withFailColor $ write "F"
}

failed_examples :: Formatter
failed_examples   = silent {
  formatterDone = defaultFailedFormatter >> defaultFooter
}

defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = do
  writeLine ""

  failures <- getFailMessages

  unless (null failures) $ do
    writeLine "Failures:"
    writeLine ""

    computeDiffs <- useDiff
    precomputedDiffs <-
      if computeDiffs
        then do
          pretty <- prettyPrintFunction
          liftIO $ mapConcurrently (precomputeDiff pretty) failures
        else return $ repeat Nothing

    forM_ (zip3 [1..] failures precomputedDiffs) $ \x -> do
      formatFailure x
      writeLine ""

    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    precomputeDiff :: Maybe PrettyPrintFunction -> FailureRecord -> IO (Maybe [Diff])
    precomputeDiff pretty (FailureRecord _ _ reason) =
      case reason of
        ExpectedButGot _ expected_ actual_ -> do
          let
            threshold = 2 :: Seconds
            (expected, actual) = case pretty of
              Just f -> f expected_ actual_
              Nothing -> (expected_, actual_)
          timeout threshold (evaluate $ diff expected actual)
        _ -> return Nothing
    formatFailure :: (Int, FailureRecord, Maybe [Diff]) -> FormatM ()
    formatFailure (n, FailureRecord mLoc path reason, precomputedDiff) = do
      unicode <- outputUnicode
      forM_ mLoc $ \loc -> do
        withInfoColor $ writeLine ("  " ++ formatLocation loc)
      write ("  " ++ show n ++ ") ")
      writeLine (formatRequirement path)
      case reason of
        NoReason -> return ()
        Reason err -> withFailColor $ indent err
        ExpectedButGot preface expected_ actual_ -> do
          pretty <- prettyPrintFunction
          let
            (expected, actual) = case pretty of
              Just f -> f expected_ actual_
              Nothing -> (expected_, actual_)

          mapM_ indent preface

          case precomputedDiff of
            Just chunks -> do
              writeDiff chunks extraChunk missingChunk
            Nothing -> do
              writeDiff [First expected, Second actual] write write
          where
            writeDiff chunks extra missing = do
              writeChunks "expected: " (expectedChunks chunks) extra
              writeChunks " but got: " (actualChunks chunks) missing

            writeChunks :: String -> [Chunk] -> (String -> FormatM ()) -> FormatM ()
            writeChunks pre chunks colorize = do
              withFailColor $ write (indentation ++ pre)
              forM_ (indentChunks indentation_ chunks) $ \ chunk -> case chunk of
                PlainChunk a -> write a
                ColorChunk a -> colorize a
              writeLine ""
              where
                indentation_ = indentation ++ replicate (length pre) ' '

        Error info e -> do
          mapM_ indent info
          withFailColor . indent $ "uncaught exception: " ++ formatException e

      writeLine ""

      let path_ = (if unicode then ushow else show) (joinPath path)
      writeLine ("  To rerun use: --match " ++ path_)
      where
        indentation = "       "
        indent message = do
          forM_ (lines message) $ \line -> do
            writeLine (indentation ++ line)

data Chunk = Original String | Modified String
  deriving (Eq, Show)

expectedChunks :: [Diff] -> [Chunk]
expectedChunks = mapMaybe $ \ chunk -> case chunk of
  Both a -> Just $ Original a
  First a -> Just $ Modified a
  Second _ -> Nothing

actualChunks :: [Diff] -> [Chunk]
actualChunks = mapMaybe $ \ chunk -> case chunk of
  Both a -> Just $ Original a
  First _ -> Nothing
  Second a -> Just $ Modified a

data ColorChunk = PlainChunk String | ColorChunk String
  deriving (Eq, Show)

indentChunks :: String -> [Chunk] -> [ColorChunk]
indentChunks indentation = concatMap $ \ chunk -> case chunk of
  Original y -> [indentOriginal indentation y]
  Modified y -> indentModified indentation y

indentOriginal :: String -> String -> ColorChunk
indentOriginal indentation = PlainChunk . go
  where
    go text = case break (== '\n') text of
      (xs, _ : ys) -> xs ++ "\n" ++ indentation ++ go ys
      (xs, "") -> xs

indentModified :: String -> String -> [ColorChunk]
indentModified indentation = go
  where
    go text = case text of
      "\n" -> [PlainChunk "\n", ColorChunk indentation]
      '\n' : ys@('\n' : _) -> PlainChunk "\n" : ColorChunk indentation : go ys
      _ -> case break (== '\n') text of
        (xs, _ : ys) -> segment xs ++ PlainChunk ('\n' : indentation) : go ys
        (xs, "") -> segment xs

    segment xs = case span isSpace $ reverse xs of
      ("", "") -> []
      ("", _) -> [ColorChunk xs]
      (_, "") -> [ColorChunk xs]
      (ys, zs) -> [ColorChunk (reverse zs), ColorChunk (reverse ys)]

defaultFooter :: FormatM ()
defaultFooter = do

  writeLine =<< (++)
    <$> (printf "Finished in %1.4f seconds" <$> getRealTime)
    <*> (maybe "" (printf ", used %1.4f seconds of CPU time") <$> getCPUTime)

  fails   <- getFailCount
  pending <- getPendingCount
  total   <- getTotalCount

  let
    output =
         pluralize total   "example"
      ++ ", " ++ pluralize fails "failure"
      ++ if pending == 0 then "" else ", " ++ show pending ++ " pending"
    c | fails /= 0   = withFailColor
      | pending /= 0 = withPendingColor
      | otherwise    = withSuccessColor
  c $ writeLine output

formatLocation :: Location -> String
formatLocation (Location file line column) = file ++ ":" ++ show line ++ ":" ++ show column ++ ": "
