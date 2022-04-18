{-# LANGUAGE DerivingStrategies #-}
module Main (main, spec) where

import           Prelude

import           Data.IORef
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import           Test.Hspec.Core.Spec hiding (it)
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Hooks
import           Test.Hspec.Expectations

newtype App a = App { unApp :: StateT Int IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runApp :: App a -> IO a
runApp = flip evalStateT 0 . unApp

modifyValue :: (Int -> Int) -> App ()
modifyValue = App . modify

value :: App Int
value = App get

main :: IO ()
main = hspec spec

spec :: Spec
spec = with runApp $ do
  before (modifyValue (+ 5)) $ do
    before (modifyValue (* 2)) $ do
      describe "App.value" $ do
        it "returns the value" $ do
          n <- value
          liftIO $ do
            n `shouldBe` 23

with :: (forall a. m a -> IO a) -> SpecWith m a -> SpecWith IO a
with run = mapSpecItem_ $ \ item -> item {
  itemExample = \ params aroundAction progress -> do
    ref <- newIORef (Result "" Success)
    safeEvaluate $ do
      aroundAction $ \ a -> run (itemExample item params ($ a) progress) >>= writeIORef ref
      readIORef ref
}

it :: HasCallStack => Monad m => String -> m () -> SpecWith m ()
it label action = fromSpecList [specItem__ label action]

specItem__ :: HasCallStack => Monad m => String -> m () -> SpecTree m ()
specItem__ label action = specItem_ label $ \ _params aroundAction _progress -> do
  aroundAction (\ () -> action) >> return (Result "" Success)
