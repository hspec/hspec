module Test.Hspec.Api.Example.V1 (
  Example(..)
, Params(..)
, ActionWith
, Progress
, ProgressCallback
, Result(..)
, ResultStatus(..)
, Location(..)
, FailureReason(..)

, liftEvaluate

, Options
, OptionsSet
, getOptions
, setOptions
, modifyOptions

, SpecWith
, modifyParams
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Example hiding (Params(..))
import qualified Test.Hspec.Core.Example as Example
import           Test.Hspec.Core.Example.Options
import           Test.Hspec.Core.Spec.Monad (SpecWith)
import qualified Test.Hspec.Core.Spec.Monad as Spec

data Params = Params {
  paramsSeed :: Integer
, paramsOptions :: OptionsSet
}

liftParams :: Example.Params -> Params
liftParams (Example.Params _ _ seed optionsSet) = Params seed optionsSet

liftEvaluate :: Example e => (e -> Params -> Opt e -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result)
                          -> (e -> Example.Params          -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result)
liftEvaluate eval e params = eval e (liftParams params) (getOptions $ Example.paramsOptions params)

modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
modifyParams f = Spec.modifyParams $ \ params -> case f (liftParams params) of
  Params seed opts -> params {
    Example.paramsSeed = seed
  , Example.paramsOptions = opts
  }
