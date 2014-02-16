{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}
module Test.Hspec.Core.Type (
  Spec
, SpecWith
, SpecM (..)
, runSpecM
, fromSpecList
, SpecTree (..)
, mapSpecTree
, Item (..)
, ActionWith
, mapSpecItem
, Location (..)
, LocationAccuracy(..)
, Example (..)
, Result (..)
, Params (..)
, Progress
, ProgressCallback

, describe
, it
, forceResult

, runIO

, pending
, pendingWith
) where

import qualified Control.Exception as E
import           Control.Applicative
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class (liftIO)
import           Data.Typeable (Typeable)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)

import           Test.Hspec.Compat
import           Test.Hspec.Util
import           Test.Hspec.Expectations
import           Test.HUnit.Lang (HUnitFailure(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.IO ()

import           Test.Hspec.Core.QuickCheckUtil
import           Control.DeepSeq (deepseq)

type Spec = SpecWith ()

type SpecWith a = SpecM a ()

-- | A writer monad for `SpecTree` forests.
newtype SpecM a r = SpecM (WriterT [SpecTree a] IO r)
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: SpecWith a -> IO [SpecTree a]
runSpecM (SpecM specs) = execWriterT specs

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree a] -> SpecWith a
fromSpecList = SpecM . tell

-- | Run an IO action while constructing the spec tree.
--
-- `SpecM` is a monad to construct a spec tree, without executing any spec
-- items.  `runIO` allows you to run IO actions during this construction phase.
-- The IO action is always run when the spec tree is constructed (e.g. even
-- when @--dry-run@ is specified).
runIO :: IO r -> SpecM a r
runIO = SpecM . liftIO

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show, Read, Typeable)

forceResult :: Result -> Result
forceResult r = case r of
  Success   -> r
  Pending m -> m `deepseq` r
  Fail    m -> m `deepseq` r

instance E.Exception Result

type Progress = (Int, Int)
type ProgressCallback = Progress -> IO ()

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Int
} deriving (Show)

-- | Internal representation of a spec.
data SpecTree a =
    SpecGroup String [SpecTree a]
  | SpecWithCleanup (IO ()) [SpecTree a]
  | SpecItem (Item a)

data Item a = Item {
  itemRequirement :: String
, itemLocation :: Maybe Location
, itemIsParallelizable :: Bool
, itemExample :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
}

-- | An `IO` action that expects an argument of type @a@.
type ActionWith a = a -> IO ()

mapSpecTree :: (SpecTree a -> SpecTree b) -> SpecWith a -> SpecWith b
mapSpecTree f spec = runIO (runSpecM spec) >>= fromSpecList . map f

mapSpecItem :: (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem f = mapSpecTree go
  where
    go spec = case spec of
      SpecGroup d xs -> SpecGroup d (map go xs)
      SpecWithCleanup cleanup xs -> SpecWithCleanup cleanup (map go xs)
      SpecItem item -> SpecItem (f item)

data LocationAccuracy = ExactLocation | BestEffort
  deriving (Eq, Show)

data Location = Location {
  locationFile :: String
, locationLine :: Int
, locationColumn :: Int
, locationAccuracy :: LocationAccuracy
} deriving (Eq, Show)

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [SpecTree a] -> SpecTree a
describe s = SpecGroup msg
  where
    msg
      | null s = "(no description given)"
      | otherwise = s

-- | Create a spec item.
it :: Example a => String -> a -> SpecTree (Arg a)
it s e = SpecItem $ Item requirement Nothing False (evaluateExample e)
  where
    requirement
      | null s = "(unspecified behavior)"
      | otherwise = s

-- | A type class for examples.
class Example e where
  type Arg e
  evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result

instance Example Bool where
  type Arg Bool = ()
  evaluateExample b _ _ _ = if b then return Success else return (Fail "")

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _ action _ = (action e >> return Success) `E.catches` [
      E.Handler (\(HUnitFailure err) -> return (Fail err))
    , E.Handler (return :: Result -> IO Result)
    ]

instance Example Result where
  type Arg Result = ()
  evaluateExample r _ _ _ = return r

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $
      case r of
        QC.Success {}               -> Success
        QC.Failure {QC.output = m}  -> fromMaybe (Fail $ sanitizeFailureMessage r) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

      sanitizeFailureMessage :: QC.Result -> String
      sanitizeFailureMessage r = let m = QC.output r in strip $
#if MIN_VERSION_QuickCheck(2,7,0)
        case QC.theException r of
          Just e -> let numbers = formatNumbers r in
            "uncaught exception: " ++ formatException e ++ " " ++ numbers ++ "\n" ++ case lines m of
              x:xs | x == (exceptionPrefix ++ show e ++ "' " ++ numbers ++ ": ") -> unlines xs
              _ -> m
          Nothing ->
#endif
            (addFalsifiable . stripFailed) m

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
        | exceptionPrefix `isPrefixOf` m = (readMaybe . takeWhile (/= '\'') . drop n) m
        | otherwise = Nothing
        where
          n = length exceptionPrefix

      exceptionPrefix = "*** Failed! Exception: '"

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
