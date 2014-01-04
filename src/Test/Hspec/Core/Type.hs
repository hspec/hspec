{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Hspec.Core.Type (
  Spec
, SpecM (..)
, runSpecM
, fromSpecList
, SpecTree (..)
, Item (..)
, Example (..)
, Result (..)
, QuickCheckArgs (..)
, SmallCheckParams(..)
, ReportProgressParam(..)
, SomeParam(..)
, Progress
, findParam
, findParamDef

, describe
, it
, forceResult

, pending
, pendingWith
) where

import qualified Control.Exception as E
import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Typeable (Typeable, cast)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)

import           Test.Hspec.Util
import           Test.Hspec.Expectations
import           Test.HUnit.Lang (HUnitFailure(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.IO ()

import           Test.Hspec.Core.QuickCheckUtil
import           Control.DeepSeq (deepseq)

type Spec = SpecM ()

-- | A writer monad for `SpecTree` forests.
newtype SpecM a = SpecM (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: Spec -> [SpecTree]
runSpecM (SpecM specs) = execWriter specs

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree] -> Spec
fromSpecList = SpecM . tell

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show, Read, Typeable)

forceResult :: Result -> Result
forceResult r = case r of
  Success   -> r
  Pending m -> r `seq` m `deepseq` r
  Fail    m -> r `seq` m `deepseq` r

instance E.Exception Result

newtype QuickCheckArgs = QuickCheckArgs {
  quickCheckArgs :: QC.Args
} deriving Typeable

newtype SmallCheckParams = SmallCheckParams {
  smallCheckDepth :: Int
} deriving Typeable

type Progress = (Int, Int)

newtype ReportProgressParam = ReportProgressParam {
  reportTestProgress :: Progress -> IO ()
} deriving Typeable

data SomeParam = forall p. Typeable p => SomeParam p

findParam :: Typeable p => [SomeParam] -> Maybe p
findParam [] = Nothing
findParam (SomeParam x:xs) = maybe (findParam xs) Just $ cast x

findParamDef :: Typeable p => p -> [SomeParam] -> p
findParamDef d = fromMaybe d . findParam

-- | Internal representation of a spec.
data SpecTree =
    SpecGroup String [SpecTree]
  | SpecItem Item
  | SpecBracket (IO SomeParam) (SomeParam -> IO ()) SpecTree

data Item = Item {
  itemIsParallelizable :: Bool
, itemRequirement :: String
, itemExample :: [SomeParam] -> (IO () -> IO ()) -> IO Result
}

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [SpecTree] -> SpecTree
describe s = SpecGroup msg
  where
    msg
      | null s = "(no description given)"
      | otherwise = s

-- | Create a spec item.
it :: Example a => String -> a -> SpecTree
it s e = SpecItem $ Item False msg (evaluateExample e)
  where
    msg
      | null s = "(unspecified behavior)"
      | otherwise = s

-- | A type class for examples.
class Example a where
  evaluateExample :: a -> [SomeParam] -> (IO () -> IO ()) -> IO Result

instance Example Bool where
  evaluateExample b _ _ = if b then return Success else return (Fail "")

instance Example Expectation where
  evaluateExample e _ action = (action e >> return Success) `E.catches` [
      E.Handler (\(HUnitFailure err) -> return (Fail err))
    , E.Handler (return :: Result -> IO Result)
    ]

instance Example Result where
  evaluateExample r _ _ = return r

instance Example QC.Property where
  evaluateExample p cs action = do
    r <- QC.quickCheckWithResult qcArgs {QC.chatty = False} (QCP.callback progressCallback $ aroundProperty action p)
    when (isUserInterrupt r) $ do
      E.throwIO E.UserInterrupt

    return $
      case r of
        QC.Success {}               -> Success
        QC.Failure {QC.output = m}  -> fromMaybe (Fail $ sanitizeFailureMessage m) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")
    where
      qcArgs = quickCheckArgs $ findParamDef (QuickCheckArgs QC.stdArgs) cs
      reportProg = reportTestProgress $ findParamDef (ReportProgressParam $ const $ return ()) cs

      progressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> reportProg (QC.numSuccessTests st, QC.maxSuccessTests st)

      sanitizeFailureMessage :: String -> String
      sanitizeFailureMessage = strip . addFalsifiable . stripFailed

      addFalsifiable :: String -> String
      addFalsifiable m
        | "(after " `isPrefixOf` m = "Falsifiable " ++ m
        | otherwise = m

      stripFailed :: String -> String
      stripFailed m
        | prefix `isPrefixOf` m = drop n m
        | otherwise = m
        where
          prefix = "*** Failed! "
          n = length prefix

      parsePending :: String -> Maybe Result
      parsePending m
        | prefix `isPrefixOf` m = (readMaybe . takeWhile (/= '\'') . drop n) m
        | otherwise = Nothing
        where
          n = length prefix
          prefix = "*** Failed! Exception: '"

-- | Specifies a pending example.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
pending :: Expectation
pending = E.throwIO (Pending Nothing)

-- | Specifies a pending example with a reason for why it's pending.
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pendingWith "waiting for clarification from the designers"
pendingWith :: String -> Expectation
pendingWith = E.throwIO . Pending . Just
