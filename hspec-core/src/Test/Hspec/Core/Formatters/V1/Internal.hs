{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Test.Hspec.Core.Formatters.V1.Internal (
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
, FailureReason (..)
, FormatM
, formatterToFormat

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

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

, useDiff
, extraChunk
, missingChunk

-- ** Helpers
, formatException
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (First)

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example (Location(..))
import           Text.Printf
import           Control.Monad.IO.Class

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.V1.Monad (
    Formatter(..)
  , FailureReason(..)
  , FormatM

  , getSuccessCount
  , getPendingCount
  , getFailCount
  , getTotalCount

  , FailureRecord(..)
  , getFailMessages
  , usedSeed

  , getCPUTime
  , getRealTime

  , write
  , writeLine
  , writeTransient

  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor

  , useDiff
  , extraChunk
  , missingChunk
  )

import           Test.Hspec.Core.Format (FormatConfig, Format)

import           Test.Hspec.Core.Formatters.Diff
import qualified Test.Hspec.Core.Formatters.V2 as V2
import           Test.Hspec.Core.Formatters.V1.Monad (Item(..), Result(..), Environment(..), interpretWith)

formatterToFormat :: Formatter -> FormatConfig -> IO Format
formatterToFormat = V2.formatterToFormat . legacyFormatterToFormatter

legacyFormatterToFormatter :: Formatter -> V2.Formatter
legacyFormatterToFormatter Formatter{..} = V2.Formatter {
  V2.formatterStarted = interpret headerFormatter
, V2.formatterGroupStarted = interpret . uncurry exampleGroupStarted
, V2.formatterGroupDone = interpret . const exampleGroupDone
, V2.formatterProgress = \ path -> interpret . exampleProgress path
, V2.formatterItemStarted = interpret . exampleStarted
, V2.formatterItemDone = \ path item -> interpret $ do
    case itemResult item of
      Success -> exampleSucceeded path (itemInfo item)
      Pending _ reason -> examplePending path (itemInfo item) reason
      Failure _ reason -> exampleFailed path (itemInfo item) (unliftFailureReason reason)
, V2.formatterDone = interpret $ failedFormatter >> footerFormatter
}

unliftFailureRecord :: V2.FailureRecord -> FailureRecord
unliftFailureRecord V2.FailureRecord{..} = FailureRecord {
  failureRecordLocation
, failureRecordPath
, failureRecordMessage = unliftFailureReason failureRecordMessage
}

unliftFailureReason :: V2.FailureReason -> FailureReason
unliftFailureReason = \ case
  V2.NoReason -> NoReason
  V2.Reason reason -> Reason reason
  V2.ColorizedReason reason -> Reason (stripAnsi reason)
  V2.ExpectedButGot preface expected actual -> ExpectedButGot preface expected actual
  V2.Error info e -> Error info e

interpret :: FormatM a -> V2.FormatM a
interpret = interpretWith Environment {
  environmentGetSuccessCount = V2.getSuccessCount
, environmentGetPendingCount = V2.getPendingCount
, environmentGetFailMessages = map unliftFailureRecord <$> V2.getFailMessages
, environmentUsedSeed = V2.usedSeed
, environmentPrintTimes = V2.printTimes
, environmentGetCPUTime = V2.getCPUTime
, environmentGetRealTime = V2.getRealTime
, environmentWrite = V2.write
, environmentWriteTransient = V2.writeTransient
, environmentWithFailColor = V2.withFailColor
, environmentWithSuccessColor = V2.withSuccessColor
, environmentWithPendingColor = V2.withPendingColor
, environmentWithInfoColor = V2.withInfoColor
, environmentUseDiff = V2.useDiff
, environmentExtraChunk = V2.extraChunk
, environmentMissingChunk = V2.missingChunk
, environmentLiftIO = liftIO
}

silent :: Formatter
silent = Formatter {
  headerFormatter     = pass
, exampleGroupStarted = \_ _ -> pass
, exampleGroupDone    = pass
, exampleStarted      = \_ -> pass
, exampleProgress     = \_ _ -> pass
, exampleSucceeded    = \ _ _ -> pass
, exampleFailed       = \_ _ _ -> pass
, examplePending      = \_ _ _ -> pass
, failedFormatter     = pass
, footerFormatter     = pass
}

checks :: Formatter
checks = specdoc {
  exampleStarted = \(nesting, requirement) -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [ ]"

, exampleProgress = \(nesting, requirement) p -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [" ++ (formatProgress p) ++ "]"

, exampleSucceeded = \(nesting, requirement) info -> do
    writeResult nesting requirement info $ withSuccessColor $ write "✔"

, exampleFailed = \(nesting, requirement) info _ -> do
    writeResult nesting requirement info $ withFailColor $ write "✘"

, examplePending = \(nesting, requirement) info reason -> do
    writeResult nesting requirement info $ withPendingColor $ write "‐"

    withPendingColor $ do
      writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult :: [String] -> String -> String -> FormatM () -> FormatM ()
    writeResult nesting requirement info action = do
      write $ indentationFor nesting ++ requirement ++ " ["
      action
      writeLine "]"
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

specdoc :: Formatter
specdoc = silent {

  headerFormatter = do
    writeLine ""

, exampleGroupStarted = \nesting name -> do
    writeLine (indentationFor nesting ++ name)

, exampleProgress = \_ p -> do
    writeTransient (formatProgress p)

, exampleSucceeded = \(nesting, requirement) info -> withSuccessColor $ do
    writeLine $ indentationFor nesting ++ requirement
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s

, exampleFailed = \(nesting, requirement) info _ -> withFailColor $ do
    n <- getFailCount
    writeLine $ indentationFor nesting ++ requirement ++ " FAILED [" ++ show n ++ "]"
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s

, examplePending = \(nesting, requirement) info reason -> withPendingColor $ do
    writeLine $ indentationFor nesting ++ requirement
    forM_ (lines info) $ \ s ->
      writeLine $ indentationFor ("" : nesting) ++ s
    writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason

, failedFormatter = defaultFailedFormatter

, footerFormatter = defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '
    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total


progress :: Formatter
progress = silent {
  exampleSucceeded = \_ _ -> withSuccessColor $ write "."
, exampleFailed    = \_ _ _ -> withFailColor    $ write "F"
, examplePending   = \_ _ _ -> withPendingColor $ write "."
, failedFormatter  = defaultFailedFormatter
, footerFormatter  = defaultFooter
}


failed_examples :: Formatter
failed_examples   = silent {
  failedFormatter = defaultFailedFormatter
, footerFormatter = defaultFooter
}

defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = do
  writeLine ""

  failures <- getFailMessages

  unless (null failures) $ do
    writeLine "Failures:"
    writeLine ""

    forM_ (zip [1..] failures) $ \x -> do
      formatFailure x
      writeLine ""

    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    formatFailure :: (Int, FailureRecord) -> FormatM ()
    formatFailure (n, FailureRecord mLoc path reason) = do
      forM_ mLoc $ \loc -> do
        withInfoColor $ writeLine (formatLoc loc)
      write ("  " ++ show n ++ ") ")
      writeLine (formatRequirement path)
      case reason of
        NoReason -> pass
        Reason err -> withFailColor $ indent err
        ExpectedButGot preface expected actual -> do
          mapM_ indent preface

          b <- useDiff

          let threshold = 2 :: Seconds

          mchunks <- liftIO $ if b
            then timeout threshold (evaluate $ diff Nothing expected actual)
            else return Nothing

          case mchunks of
            Just chunks -> do
              writeDiff chunks extraChunk missingChunk
            Nothing -> do
              writeDiff [First expected, Second actual] write write
          where
            indented output text = case break (== '\n') text of
              (xs, "") -> output xs
              (xs, _ : ys) -> output (xs ++ "\n") >> write (indentation ++ "          ") >> indented output ys

            writeDiff chunks extra missing = do
              withFailColor $ write (indentation ++ "expected: ")
              forM_ chunks $ \ case
                Both a -> indented write a
                First a -> indented extra a
                Second _ -> pass
                Omitted _ -> pass
              writeLine ""

              withFailColor $ write (indentation ++ " but got: ")
              forM_ chunks $ \ case
                Both a -> indented write a
                First _ -> pass
                Second a -> indented missing a
                Omitted _ -> pass
              writeLine ""

        Error _ e -> withFailColor . indent $ (("uncaught exception: " ++) . formatException) e

      writeLine ""
      writeLine ("  To rerun use: --match " ++ show (joinPath path))
      where
        indentation = "       "
        indent message = do
          forM_ (lines message) $ \line -> do
            writeLine (indentation ++ line)
        formatLoc (Location file line column) = "  " ++ file ++ ":" ++ show line ++ ":" ++ show column ++ ": "

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
