{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Stability: experimental
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Runner.hspecWith`.
module Test.Hspec.Core.Formatters (

-- * Formatters
  silent
, specdoc
, specdyn
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
, TextBlock
, TextBlockHandle
, writeTextBlock
, rewrite
-- for backwards API compatibility
, writeLine

-- ** Dealing with colors
, withStyle
, Style(..)
, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, useDiff

-- ** Helpers
, formatException

-- ** Develop
, trace
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Maybe
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Spec (Location(..))
import           Text.Printf

-- We use an explicit import list for "Test.Hspec.Formatters.Internal", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.Monad (
    Formatter (..)
  , FailureReason (..)
  , FormatM

  , getSuccessCount
  , getPendingCount
  , getFailCount
  , getTotalCount

  , FailureRecord (..)
  , getFailMessages
  , usedSeed

  , getCPUTime
  , getRealTime

  , writeLine
  , writeTextBlock
  , insertTextBlock
  , rewrite

  , TextBlock
  , TextBlockHandle
  , line
  , lineS
  , Style(..)
  , withStyle
  , writeTextBlock

  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor

  , useDiff
  )
import GHC.Exts (IsString(..))
import           Test.Hspec.Core.Clock (Seconds(..))
import           Test.Hspec.Core.Formatters.Diff

trace :: Formatter
trace = silent {
  headerFormatter     = writeLine "headerFormatter"
, exampleGroupStarted = \tags name -> writeLine $ "group started " ++ show (tags, name)
, exampleGroupDone    = \tags name -> writeLine $ "group done " ++ show (tags, name)
, exampleProgress     = \path prog -> writeLine $ "progress" ++ show (path,prog)
, exampleStarted      = \path -> writeLine $ "started" ++ show path
, exampleSucceeded    = \path name -> writeLine $ "success" ++ show (path, name)
, exampleFailed       = \path name _reason -> writeLine $ "failure" ++ show (path,name)
, examplePending      = \path name _reason -> writeLine $ "pending" ++ show (path,name)
, failedFormatter     = writeLine "failed"
, footerFormatter     = writeLine "footer"
                  }

silent :: Formatter
silent = Formatter {
  headerFormatter     = return ()
, exampleGroupStarted = \_ _ -> return ()
, exampleGroupDone    = \_ _ -> return ()
, exampleStarted      = \_ -> return ()
, exampleProgress     = \_ _ -> return ()
, exampleSucceeded    = \ _ _ -> return ()
, exampleFailed       = \_ _ _ -> return ()
, examplePending      = \_ _ _ -> return ()
, failedFormatter     = return ()
, footerFormatter     = return ()
, capturedOutputFormatter = Nothing
}

specdoc :: IO Formatter
specdoc = do
  transientRef <- liftIO $ newIORef Nothing
  let writeTransient tb = do
        hasTransient <- liftIO $ readIORef transientRef
        case hasTransient of
          Nothing -> do
            idx <- writeTextBlock tb
            liftIO $ writeIORef transientRef (Just idx)
          Just idx ->
            rewrite idx (const $ Just tb)
      clearTransient = do
        hasTransient <- liftIO $ atomicModifyIORef' transientRef $ \x -> (Nothing, x)
        case hasTransient of
          Just idx ->
            rewrite idx (const Nothing)
          Nothing ->
            return ()

      reportGroupStarted nesting name =
        writeLine (indentationFor nesting ++ name)

      reportSuccess ex@(nesting, _) info = do
          clearTransient
          void $ writeTextBlock $ do
            line $ withSuccessColor (renderExampleLine ex)
            forM_ (lines info) $ \s ->
              lineS $ indentationFor ("" : nesting) ++ s
      reportFailure ex@(nesting, _) info = do
          clearTransient
          n <- getFailCount
          void $ writeTextBlock $ do
            line $ withFailColor $ renderExampleLine ex ++ " FAILED [" ++ show n ++ "]"
            forM_ (lines info) $ \ s ->
              lineS $ indentationFor ("" : nesting) ++ s
      reportPending ex@(nesting, _) info reason = do
          clearTransient
          void $ writeTextBlock $ do
            line $ withPendingColor $ renderExampleLine ex
            forM_ (lines info) $ \ s ->
              lineS $ indentationFor ("" : nesting) ++ s
            lineS $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
  return silent {
      headerFormatter = do
        writeLine ""

    , exampleGroupStarted = \nesting name ->
        reportGroupStarted nesting name

    , exampleProgress = \_path p ->
        writeTransient (fromString $ formatProgress p)

    , exampleSucceeded = reportSuccess

    , exampleFailed = \path info _ ->
        reportFailure path info

    , examplePending = \path info reason ->
        reportPending path info reason

    , failedFormatter = defaultFailedFormatter

    , footerFormatter = defaultFooter
    } where
    indentationFor nesting = replicate (length nesting * 2) ' '
    renderExampleLine (nesting, requirement) = indentationFor nesting ++ requirement
    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total


specdyn :: IO Formatter
specdyn = do
  runningRef <- newIORef Map.empty
  groupsRef  <- newIORef Map.empty
  sdoc <- specdoc
  return sdoc {
      exampleStarted = \ex@(group, _) -> do
        gg <- liftIO $ readIORef groupsRef
        l  <- case Map.lookup group gg of
          Nothing -> writeTextBlock $ fromString $ renderExampleLine ex
          Just gl -> insertTextBlock gl $ fromString $ renderExampleLine ex
        liftIO $ modifyIORef runningRef (Map.insert ex l)
        liftIO $ modifyIORef groupsRef (Map.insert group l)

    , exampleGroupStarted = \nesting name -> do
        h <- writeTextBlock $ fromString(indentationFor nesting ++ name)
        liftIO $ modifyIORef groupsRef (Map.insert (nesting ++ [name]) h)

    , exampleProgress = \ex p -> do
        running <- liftIO $ readIORef runningRef
        let prevLine = Map.lookup ex running
        case prevLine of
          Nothing -> error "exampleProgress" -- exampleProgress cannot arise before exampleStarted
          Just l  -> rewrite l $ const $ Just $ fromString $ renderExampleLine ex ++ ' ':formatProgress p

    , exampleSucceeded = \ex@(nesting, _) info -> do
        prevLine <- liftIO $ atomicModifyIORef' runningRef $ \m -> let (a,m') = Map.updateLookupWithKey (\_ _ -> Nothing) ex m in (m',a)
        let output = case prevLine of
              Just l -> rewrite l . const . Just
              Nothing -> void . writeTextBlock
        output $ do
          line $ withSuccessColor (renderExampleLine ex)
          forM_ (lines info) $ \s ->
            lineS $ indentationFor ("" : nesting) ++ s

    , exampleFailed = \ex@(nesting, _) info _ -> do
        n <- getFailCount
        prevLine <- liftIO $ atomicModifyIORef' runningRef $ \m -> let (a,m') = Map.updateLookupWithKey (\_ _ -> Nothing) ex m in (m',a)
        let output = case prevLine of
              Just l -> rewrite l . const . Just
              Nothing -> void . writeTextBlock
        output $ do
          line $ withFailColor $ renderExampleLine ex ++ " FAILED [" ++ show n ++ "]"
          forM_ (lines info) $ \ s ->
            lineS $ indentationFor ("" : nesting) ++ s

    , examplePending = \ex@(nesting, _) info reason -> void $ do
        prevLine <- liftIO $ atomicModifyIORef' runningRef $ \m -> let (a,m') = Map.updateLookupWithKey (\_ _ -> Nothing) ex m in (m',a)
        let output = case prevLine of
              Just l -> rewrite l . const . Just
              Nothing -> void . writeTextBlock
        output $ do
          line $ withPendingColor $ renderExampleLine ex
          forM_ (lines info) $ \ s ->
            lineS $ indentationFor ("" : nesting) ++ s
          lineS $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason

    , capturedOutputFormatter = Just $ \str ->
        unless (null str) $ do
          writeLine "PROGRAM OUTPUT:"
          writeLine str
    }
  where
    indentationFor nesting = replicate (length nesting * 2) ' '
    renderExampleLine (nesting, requirement) = indentationFor nesting ++ requirement
    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total


progress :: IO Formatter
progress = do
  runningRef <- newIORef Nothing
  let appendRunning ts = do
        running <- liftIO $ readIORef runningRef
        case running of
          Nothing -> do
            h <- writeTextBlock ts
            liftIO $ writeIORef runningRef (Just h)
            return ()
          Just h ->
            rewrite h (Just . (<> ts))
  return silent {
      exampleSucceeded = \_ _   -> appendRunning $ withSuccessColor "."
    , exampleFailed    = \_ _ _ -> appendRunning $ withFailColor    "F"
    , examplePending   = \_ _ _ -> appendRunning $ withPendingColor "."
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
  failures <- getFailMessages
  seed <- usedSeed
  b <- useDiff
  void $ writeTextBlock $ do
    line ""
    unless (null failures) $ do
      line "Failures:"
      line ""

      forM_ (zip [1..] failures) $ \x -> do
        formatFailure b x
        line ""

#if __GLASGOW_HASKELL__ == 800
      withFailColor $ do
        line "WARNING:"
        line "  Your version of GHC is affected by https://ghc.haskell.org/trac/ghc/ticket/13285."
        line "  Source locations may not work as expected."
        line ""
        line "  Please consider upgrading GHC!"
        line ""
#endif

      lineS $ "Randomized with seed " ++ show seed
      line ""
  where
    formatFailure :: Bool -> (Int, FailureRecord) -> TextBlock ()
    formatFailure doDiff (n, FailureRecord mLoc path reason) = do
      forM_ mLoc $ \loc -> do
        line $ withInfoColor $ formatLoc loc
      lineS $ "  " ++ show n ++ ") " ++ formatRequirement path
      case reason of
        NoReason -> return ()
        Reason err -> withFailColor $ indent err
        ExpectedButGot preface expected actual -> do
          mapM_ lineS $ fmap indent preface
          let chunks
                | doDiff = diff expected actual
                | otherwise = [First expected, Second actual]

          withFailColor $ indentation ++ "expected: "
          forM_ chunks $ \chunk -> case chunk of
            Both a _ -> indented PlainStyle a
            First a -> indented DiffExtraStyle a
            Second _ -> return ()
          line ""

          withFailColor $ indentation ++ " but got: "
          forM_ chunks $ \chunk -> case chunk of
            Both a _ -> indented PlainStyle a
            First _ -> return ()
            Second a -> indented DiffMissingStyle a
          line ""
          where
            indented :: Style -> String -> TextBlock ()
            indented style text = case break (== '\n') text of
              (xs, "") -> withStyle style xs
              (xs, _ : ys) -> do
                line $ withStyle style xs
                fromString (indentation ++ "          ")
                indented style ys
        Error _ e -> withFailColor . indent $ (("uncaught exception: " ++) . formatException) e

      line ""
      line $ fromString ("  To rerun use: --match " ++ show (joinPath path))
      where
        indentation = "       "
        indent message = unlines $ map (indentation ++) (lines message)
        formatLoc (Location file l column) = "  " ++ file ++ ":" ++ show l ++ ":" ++ show column ++ ": "

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
  void $ writeTextBlock $ c output
