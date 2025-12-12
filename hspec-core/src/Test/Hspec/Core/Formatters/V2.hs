{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Stability: unstable
--
-- This is an unstable API.  Use
-- [Test.Hspec.Api.Formatters.V3](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Formatters-V3.html)
-- instead.
module Test.Hspec.Core.Formatters.V2
-- {-# WARNING "Use [Test.Hspec.Api.Formatters.V3](https://hackage.haskell.org/package/hspec-api/docs/Test-Hspec-Api-Formatters-V3.html) instead." #-}
(
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
, Path
, Progress
, Location(..)
, Item(..)
, Result(..)
, FailureReason (..)
, FormatM
, formatterToFormat

-- ** Accessing config values
, getConfig
, getConfigValue
, FormatConfig(..)

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
, diffContext
, externalDiffAction
, prettyPrint
, prettyPrintFunction
, extraChunk
, missingChunk

-- ** expert mode
, unlessExpert

-- ** Helpers
, formatLocation
, Util.formatException

#ifdef TEST
, Chunk(..)
, ColorChunk(..)
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           System.IO (hFlush, stdout)

import           Test.Hspec.Core.Util hiding (formatException)
import qualified Test.Hspec.Core.Util as Util
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Example (Location(..), Progress)
import           Text.Printf
import           Test.Hspec.Core.Formatters.Pretty.Unicode (ushow)
import           Control.Monad.IO.Class

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

  , getConfig
  , getConfigValue
  , FormatConfig(..)

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
  , diffContext
  , externalDiffAction
  , prettyPrint
  , prettyPrintFunction
  , extraChunk
  , missingChunk

  , unlessExpert
  )

import           Test.Hspec.Core.Formatters.Diff

silent :: Formatter
silent = Formatter {
  formatterStarted      = pass
, formatterGroupStarted = \ _ -> pass
, formatterGroupDone    = \ _ -> pass
, formatterProgress     = \ _ _ -> pass
, formatterItemStarted  = \ _ -> pass
, formatterItemDone     = \ _ _ -> pass
, formatterDone         = pass
}

checks :: Formatter
checks = specdoc {
  formatterProgress = \(nesting, requirement) p -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [" ++ formatProgress p ++ "]"

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
      Success {} -> pass
      Failure {} -> pass
      Pending _ reason -> withPendingColor $ do
        indentBy (indentationFor ("" : nesting)) $ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult :: [String] -> String -> Seconds -> String -> (FormatM () -> FormatM ()) -> String -> FormatM ()
    writeResult nesting requirement duration info withColor symbol = do
      shouldPrintTimes <- printTimes
      write $ indentationFor nesting ++ requirement ++ " ["
      withColor $ write symbol
      writeLine $ "]" ++ if shouldPrintTimes then times else ""
      indentBy (indentationFor ("" : nesting)) info
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
        indentBy (indentationFor ("" : nesting)) $ "# PENDING: " ++ fromMaybe "No reason given" reason
      Failure {} -> withFailColor $ do
        n <- getFailCount
        writeResult nesting (requirement ++ " FAILED [" ++ show n ++ "]") duration info

, formatterDone = defaultFailedFormatter >> defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult nesting requirement (Seconds duration) info = do
      shouldPrintTimes <- printTimes
      writeLine $ indentationFor nesting ++ requirement ++ if shouldPrintTimes then times else ""
      indentBy (indentationFor ("" : nesting)) info
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
  formatterItemDone = \ _ item -> do
    case itemResult item of
      Success{} -> withSuccessColor $ write "."
      Pending{} -> withPendingColor $ write "."
      Failure{} -> withFailColor $ write "F"
    liftIO $ hFlush stdout
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

    forM_ (zip [1..] failures) $ \x -> do
      formatFailure x
      writeLine ""

    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    formatFailure :: (Int, FailureRecord) -> FormatM ()
    formatFailure (n, FailureRecord mLoc path reason) = do
      unicode <- outputUnicode
      forM_ mLoc $ \loc -> do
        withInfoColor $ writeLine ("  " ++ formatLocation loc)
      write ("  " ++ show n ++ ") ")
      writeLine (formatRequirement path)
      case reason of
        NoReason -> pass
        Reason err -> withFailColor $ indent err
        ColorizedReason err -> indent err
        ExpectedButGot preface expected_ actual_ -> do
          pretty <- prettyPrintFunction
          let
            (expected, actual) = case pretty of
              Just f -> f expected_ actual_
              Nothing -> (expected_, actual_)

          mapM_ indent preface

          b <- useDiff

          let threshold = 2 :: Seconds


          mExternalDiff <- externalDiffAction

          case mExternalDiff of
            Just externalDiff -> do
              liftIO $ externalDiff expected actual

            Nothing -> do
              context <- diffContext
              mchunks <- liftIO $ if b
                then timeout threshold (evaluate $ lineDiff context expected actual)
                else return Nothing

              case mchunks of
                Just chunks -> do
                  writeDiff chunks extraChunk missingChunk
                Nothing -> do
                  writeDiff [LinesFirst (splitLines expected), LinesSecond (splitLines actual)] write write
          where
            writeDiff :: [LineDiff] -> (String -> FormatM ()) -> (String -> FormatM ()) -> FormatM ()
            writeDiff chunks extra missing = do
              writeChunks "expected: " (expectedChunks chunks) extra
              writeChunks " but got: " (actualChunks chunks) missing

            writeChunks :: String -> [Chunk] -> (String -> FormatM ()) -> FormatM ()
            writeChunks pre chunks colorize = do
              withFailColor $ write (indentation ++ pre)
              go pass chunks
              where
                indentation_ :: [Char]
                indentation_ = indentation ++ replicate (length pre) ' '

                go :: FormatM () -> [Chunk] -> FormatM ()
                go indent_ = \ case
                  [] -> pass
                  c : cs -> do
                    indent_
                    case c of
                      Original a -> write a
                      Modified a -> colorize a
                      Info text -> withInfoColor $ write text
                      ModifiedChunks xs -> for_ xs $ \ case
                        PlainChunk a -> write a
                        ColorChunk a -> colorize a
                    write "\n"
                    go (write indentation_) cs

        Error info e -> do
          mapM_ indent info
          formatException <- getConfigValue formatConfigFormatException
          withFailColor . indent $ "uncaught exception: " ++ formatException e


      unlessExpert $ do
        let path_ = (if unicode then ushow else show) (joinPath path)
        writeLine ""
        seed <- usedSeed
        writeLine ("  To rerun use: --match " ++ path_ <> " --seed " <> show seed)
      where
        indentation = "       "
        indent = indentBy indentation

indentBy :: String -> String -> FormatM ()
indentBy indentation message = do
  forM_ (lines message) $ \ line -> do
    writeLine (indentation ++ line)

data Chunk = Original String | Modified String | Info String | ModifiedChunks [ColorChunk]
  deriving (Eq, Show)

expectedChunks :: [LineDiff] -> [Chunk]
expectedChunks = concatMap $ \ case
  LinesBoth a -> map Original a
  LinesFirst a -> map Modified a
  LinesSecond _ -> []
  LinesOmitted n -> [Info $ formatOmittedLines n]
  SingleLineDiff diffs -> return . ModifiedChunks . flip mapMaybe diffs $ \ case
    First a -> Just $ ColorChunk a
    Second _ -> Nothing
    Both a -> Just $ PlainChunk a

actualChunks :: [LineDiff] -> [Chunk]
actualChunks = concatMap $ \ case
  LinesBoth a -> map Original a
  LinesFirst _ -> []
  LinesSecond a -> map Modified a
  LinesOmitted n -> [Info $ formatOmittedLines n]
  SingleLineDiff diffs -> return . ModifiedChunks . flip mapMaybe diffs $ \ case
    First _ -> Nothing
    Second a -> Just $ ColorChunk a
    Both a -> Just $ PlainChunk a

data ColorChunk = PlainChunk String | ColorChunk String
  deriving (Eq, Show)

formatOmittedLines :: Int -> String
formatOmittedLines n = "@@ " <> show n <> " lines omitted @@"

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

    color
      | fails /= 0   = withFailColor
      | pending /= 0 = withPendingColor
      | otherwise    = withSuccessColor
  color $ writeLine output

formatLocation :: Location -> String
formatLocation (Location file line column) = file ++ ":" ++ show line ++ ":" ++ show column ++ ": "
