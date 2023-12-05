{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- | Stability: provisional
module Test.Hspec.Core.QuickCheck (
  modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize
, modifyMaxShrinks
, modifyArgs
, modifyOptions
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.QuickCheck (Args(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.State as QC (numSuccessTests, maxSuccessTests)
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.Util (formatException)
import           Test.Hspec.Core.Example (hunitFailureToResult)

import           Test.Hspec.Api.Example.V1 hiding (modifyOptions)
import qualified Test.Hspec.Api.Example.V1 as Example (modifyOptions)

import           Test.Hspec.Core.QuickCheck.Util
import           Test.Hspec.Core.QuickCheck.Options (QuickCheckOptions)
import qualified Test.Hspec.Core.QuickCheck.Options as Options

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSuccess = modifyOptions . Options.modifyMaxSuccess

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxDiscardRatio = modifyOptions . Options.modifyMaxDiscardRatio

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSize = modifyOptions . Options.modifyMaxSize

-- | Use a modified `maxShrinks` for given spec.
modifyMaxShrinks :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxShrinks = modifyOptions . Options.modifyMaxShrinks

-- | Use modified `Args` for given spec.
modifyArgs :: (Args -> Args) -> SpecWith a -> SpecWith a
modifyArgs f = modifyOptions $ \ options -> options { Options.modifyArgs = f . Options.modifyArgs options }

-- | Use modified `QuickCheckOptions` for given spec.
modifyOptions :: (QuickCheckOptions -> QuickCheckOptions) -> SpecWith a -> SpecWith a
modifyOptions f = modifyParams $ \ params -> params { paramsOptions = (Example.modifyOptions f $ paramsOptions params) }

instance Example QC.Property where
  type Arg QC.Property = ()
  type Opt QC.Property = QuickCheckOptions
  evaluateExample = liftEvaluate $ \ e -> evaluateProperty (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  type Opt (a -> QC.Property) = QuickCheckOptions
  evaluateExample = liftEvaluate evaluateProperty

evaluateProperty :: (a -> QCP.Property) -> Params -> QuickCheckOptions -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
evaluateProperty p params options hook progressCallback = do
  let
    args = Options.toQuickCheckArgs (paramsSeed params) options
  r <- QC.quickCheckWithResult args {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty hook p)
  return $ fromQuickCheckResult args r
  where
    qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
      \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

fromQuickCheckResult :: QC.Args -> QC.Result -> Result
fromQuickCheckResult args r = case parseQuickCheckResult r of
  QuickCheckResult _ info (QuickCheckOtherFailure err) -> Result info $ Failure Nothing (Reason err)
  QuickCheckResult _ info QuickCheckSuccess -> Result (if QC.chatty args then info else "") Success
  QuickCheckResult n info (QuickCheckFailure QCFailure{..}) -> case quickCheckFailureException of
    Just e | Just result <- fromException e -> Result info result
    Just e | Just hunit <- fromException e -> Result info $ hunitFailureToResult (Just hunitAssertion) hunit
    Just e -> failure (uncaughtException e)
    Nothing -> failure falsifiable
    where
      failure = Result info . Failure Nothing . Reason

      numbers = formatNumbers n quickCheckFailureNumShrinks

      hunitAssertion :: String
      hunitAssertion = intercalate "\n" [
          "Falsifiable " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

      uncaughtException e = intercalate "\n" [
          "uncaught exception: " ++ formatException e
        , numbers
        , indent (unlines quickCheckFailureCounterexample)
        ]

      falsifiable = intercalate "\n" [
          quickCheckFailureReason ++ " " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines
