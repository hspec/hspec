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
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.QuickCheck (Args(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.State as QC (numSuccessTests, maxSuccessTests)
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.QuickCheck.Util
import           Test.Hspec.Core.Example (Example(..), Params(..), Result(..), ResultStatus(..), FailureReason(..), hunitFailureToResult)
import           Test.Hspec.Core.Spec.Monad (SpecWith, modifyParams)

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSuccess = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSuccess = f (maxSuccess args)}

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxDiscardRatio = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxDiscardRatio = f (maxDiscardRatio args)}

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSize = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSize = f (maxSize args)}

-- | Use a modified `maxShrinks` for given spec.
modifyMaxShrinks :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxShrinks = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxShrinks = f (maxShrinks args)}

-- | Use modified `Args` for given spec.
modifyArgs :: (Args -> Args) -> SpecWith a -> SpecWith a
modifyArgs = modifyParams . modify
  where
    modify :: (Args -> Args) -> Params -> Params
    modify f p = p {paramsQuickCheckArgs = f (paramsQuickCheckArgs p)}

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  evaluateExample p params hook progressCallback = do
    let args = paramsQuickCheckArgs params
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
