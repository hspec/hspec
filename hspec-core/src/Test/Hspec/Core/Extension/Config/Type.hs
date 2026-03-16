{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Extension.Config.Type (
  Option(..)
, Config(..)
, to
, from

, setAnnotation
, getAnnotation

, addSpecTransformation
, applySpecTransformation
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified GetOpt.Declarative as Declarative

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Config.Definition (ColorMode(..), UnicodeMode(..))
import qualified Test.Hspec.Core.Config.Definition as Core
import           Test.Hspec.Core.Annotations (Annotations)
import qualified Test.Hspec.Core.Annotations as Annotations

import           Test.Hspec.Core.Extension.Tree (SpecTree)

newtype Option = Option { unOption :: Declarative.Option Config }

data Config = Config {
  ignoreConfigFile :: Bool
, dryRun :: Bool
, focusedOnly :: Bool
, failOnEmpty :: Bool
, failOnFocused :: Bool
, failOnPending :: Bool
, failOnEmptyDescription :: Bool
, printSlowItems :: Maybe Int
, printCpuTime :: Bool
, failFast :: Bool
, randomize :: Bool
, seed :: Maybe Integer
, failureReport :: Maybe FilePath
, rerun :: Bool
, rerunAllOnSuccess :: Bool

-- |
-- A predicate that is used to filter the spec before it is run.  Only examples
-- that satisfy the predicate are run.
, filterPredicate :: Maybe (Path -> Bool)
, skipPredicate :: Maybe (Path -> Bool)
, quickCheckMaxSuccess :: Maybe Int
, quickCheckMaxDiscardRatio :: Maybe Int
, quickCheckMaxSize :: Maybe Int
, quickCheckMaxShrinks :: Maybe Int
, smallCheckDepth :: Maybe Int
, colorMode :: ColorMode
, unicodeMode :: UnicodeMode
, diff :: Bool
, diffContext :: Maybe Int

-- |
-- An action that is used to print diffs.  The first argument is the value of
-- `configDiffContext`.  The remaining two arguments are the @expected@ and
-- @actual@ value.
, externalDiff :: Maybe (Maybe Int -> String -> String -> IO ())

, prettyPrint :: Bool
, prettyPrintFunction :: Bool -> String -> String -> (String, String)
, formatException :: SomeException -> String
, times :: Bool
, expertMode :: Bool
, htmlOutput :: Bool
, concurrentJobs :: Maybe Int
, annotations :: Annotations
}

to :: Config -> Core.Config
to Config{..} = Core.Config {
  configIgnoreConfigFile = ignoreConfigFile
, configDryRun = dryRun
, configFocusedOnly = focusedOnly
, configFailOnEmpty = failOnEmpty
, configFailOnFocused = failOnFocused
, configFailOnPending = failOnPending
, configFailOnEmptyDescription = failOnEmptyDescription
, configPrintSlowItems = printSlowItems
, configPrintCpuTime = printCpuTime
, configFailFast = failFast
, configRandomize = randomize
, configSeed = seed
, configFailureReport = failureReport
, configRerun = rerun
, configRerunAllOnSuccess = rerunAllOnSuccess
, configFilterPredicate = filterPredicate
, configSkipPredicate = skipPredicate
, configQuickCheckMaxSuccess = quickCheckMaxSuccess
, configQuickCheckMaxDiscardRatio = quickCheckMaxDiscardRatio
, configQuickCheckMaxSize = quickCheckMaxSize
, configQuickCheckMaxShrinks = quickCheckMaxShrinks
, configSmallCheckDepth = smallCheckDepth
, configColorMode = colorMode
, configUnicodeMode = unicodeMode
, configDiff = diff
, configDiffContext = diffContext
, configExternalDiff = externalDiff
, configPrettyPrint = prettyPrint
, configPrettyPrintFunction = prettyPrintFunction
, configFormatException = formatException
, configTimes = times
, configExpertMode = expertMode
, configAvailableFormatters = availableFormatters
, configFormat = format
, configHtmlOutput = htmlOutput
, configConcurrentJobs = concurrentJobs
, configAnnotations = annotations

, configQuickCheckSeed = Nothing
, configFormatter = Nothing
} where
    Formatters availableFormatters format = getFormatters annotations

from :: Core.Config -> Config
from config@Core.Config{..} = Config{
  ignoreConfigFile = configIgnoreConfigFile
, dryRun = configDryRun
, focusedOnly = configFocusedOnly
, failOnEmpty = configFailOnEmpty
, failOnFocused = configFailOnFocused
, failOnPending = configFailOnPending
, failOnEmptyDescription = configFailOnEmptyDescription
, printSlowItems = configPrintSlowItems
, printCpuTime = configPrintCpuTime
, failFast = configFailFast
, randomize = configRandomize
, seed = Core.getSeed config
, failureReport = configFailureReport
, rerun = configRerun
, rerunAllOnSuccess = configRerunAllOnSuccess
, filterPredicate = configFilterPredicate
, skipPredicate = configSkipPredicate
, quickCheckMaxSuccess = configQuickCheckMaxSuccess
, quickCheckMaxDiscardRatio = configQuickCheckMaxDiscardRatio
, quickCheckMaxSize = configQuickCheckMaxSize
, quickCheckMaxShrinks = configQuickCheckMaxShrinks
, smallCheckDepth = configSmallCheckDepth
, colorMode = configColorMode
, unicodeMode = configUnicodeMode
, diff = configDiff
, diffContext = configDiffContext
, externalDiff = configExternalDiff
, prettyPrint = configPrettyPrint
, prettyPrintFunction = configPrettyPrintFunction
, formatException = configFormatException
, times = configTimes
, expertMode = configExpertMode
, htmlOutput = configHtmlOutput
, concurrentJobs = configConcurrentJobs
, annotations = setFormatters (Formatters configAvailableFormatters (Core.getFormatter config)) configAnnotations
}

data Formatters = Formatters [(String, FormatConfig -> IO Format)] (Maybe (FormatConfig -> IO Format))

getFormatters :: Annotations -> Formatters
getFormatters = fromMaybe (Formatters [] Nothing) . Annotations.getValue

setFormatters :: Formatters -> Annotations -> Annotations
setFormatters = Annotations.setValue

setAnnotation :: Typeable value => value -> Config -> Config
setAnnotation value config = config { annotations = Annotations.setValue value $ annotations config }

getAnnotation :: Typeable value => Config -> Maybe value
getAnnotation = Annotations.getValue . annotations

newtype SpecTransformation = SpecTransformation { unSpecTransformation :: Config -> [SpecTree] -> [SpecTree] }

setSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
setSpecTransformation = setAnnotation . SpecTransformation

getSpecTransformation :: Config -> Config -> [SpecTree] -> [SpecTree]
getSpecTransformation = maybe (\ _ -> id) unSpecTransformation . getAnnotation

addSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
addSpecTransformation f config = setSpecTransformation (\ c -> f c . getSpecTransformation config c) config

applySpecTransformation :: Core.Config -> [SpecTree] -> [SpecTree]
applySpecTransformation (from -> config) = getSpecTransformation config config
