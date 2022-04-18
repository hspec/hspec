{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Spec.Monad (
-- RE-EXPORTED from Test.Hspec.Core.Spec
  Spec
, SpecWith
, SpecWith_
, SpecM (SpecM)
, runSpecM
, fromSpecList
, runIO

, mapSpecForest
, mapSpecItem
, mapSpecItem_
, modifyParams

, modifyConfig
-- END RE-EXPORTED from Test.Hspec.Core.Spec

, Env(..)
, withEnv
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Arrow
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree

import           Test.Hspec.Core.Config.Definition (Config)

type Spec = SpecWith_ ()

type SpecWith_ a = SpecM IO a ()

type SpecWith m a = SpecM m a ()

-- |
-- @since 2.10.0
modifyConfig :: (Config -> Config) -> SpecWith m a
modifyConfig f = SpecM $ tell (Endo f, mempty)

-- | A writer monad for `SpecTree` forests
newtype SpecM m a r = SpecM { unSpecM :: WriterT (Endo Config, [SpecTree m a]) (ReaderT Env IO) r }
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: SpecWith m a -> IO (Endo Config, [SpecTree m a])
runSpecM = flip runReaderT (Env []) . execWriterT . unSpecM

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecForest :: (Endo Config, [SpecTree m a]) -> SpecWith m a
fromSpecForest = SpecM . tell

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree m a] -> SpecWith m a
fromSpecList = fromSpecForest . (,) mempty

-- | Run an IO action while constructing the spec tree.
--
-- `SpecM` is a monad to construct a spec tree, without executing any spec
-- items.  @runIO@ allows you to run IO actions during this construction phase.
-- The IO action is always run when the spec tree is constructed (e.g. even
-- when @--dry-run@ is specified).
-- If you do not need the result of the IO action to construct the spec tree,
-- `Test.Hspec.Core.Hooks.beforeAll` may be more suitable for your use case.
runIO :: IO r -> SpecM m a r
runIO = SpecM . liftIO

mapSpecForest :: ([SpecTree m a] -> [SpecTree n b]) -> SpecM m a r -> SpecM n b r
mapSpecForest f (SpecM specs) = SpecM (mapWriterT (fmap (fmap (second f))) specs)

mapSpecItem :: (ActionWith m a -> ActionWith n b) -> (Item m a -> Item n b) -> SpecWith m a -> SpecWith n b
mapSpecItem _ = mapSpecItem_

mapSpecItem_ :: (Item m a -> Item n b) -> SpecWith m a -> SpecWith n b
mapSpecItem_ = mapSpecForest . bimapForest id

modifyParams :: (Params -> Params) -> SpecWith m a -> SpecWith m a
modifyParams f = mapSpecItem_ $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

newtype Env = Env {
  envSpecDescriptionPath :: [String]
}

withEnv :: (Env -> Env) -> SpecM m a r -> SpecM m a r
withEnv f = SpecM . WriterT . local f . runWriterT . unSpecM
