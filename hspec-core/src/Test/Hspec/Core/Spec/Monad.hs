{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Spec.Monad (
-- RE-EXPORTED from Test.Hspec.Core.Spec
  Spec
, SpecWith
, SpecM (SpecM)
, runSpecM
, fromSpecList
, runIO

, mapSpecForest
, mapSpecItem
, mapSpecItem_
, modifyParams

, modifyConfig

, withValue
, askValue
-- END RE-EXPORTED from Test.Hspec.Core.Spec

, Env(..)
, withEnv
, asks
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (local, ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.Writer
import           Data.Typeable (Typeable)

import           Test.Hspec.Core.Config.Definition (Config)
import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Annotations (Annotations)
import qualified Test.Hspec.Core.Annotations as Annotations
import           Test.Hspec.Core.Tree

type Spec = SpecWith ()

type SpecWith a = SpecM a ()

-- |
-- @since 2.10.0
modifyConfig :: (Config -> Config) -> SpecWith a
modifyConfig f = SpecM $ tell (Endo f, mempty)

-- | A writer monad for `SpecTree` forests
newtype SpecM a r = SpecM { unSpecM :: WriterT (Endo Config, [SpecTree a]) (ReaderT Env IO) r }
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: SpecWith a -> IO (Endo Config, [SpecTree a])
runSpecM = flip runReaderT (Env mempty mempty) . execWriterT . unSpecM

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecForest :: (Endo Config, [SpecTree a]) -> SpecWith a
fromSpecForest = SpecM . tell

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree a] -> SpecWith a
fromSpecList = fromSpecForest . (,) mempty

-- | Run an IO action while constructing the spec tree.
--
-- `SpecM` is a monad to construct a spec tree, without executing any spec
-- items.  @runIO@ allows you to run IO actions during this construction phase.
-- The IO action is always run when the spec tree is constructed (e.g. even
-- when @--dry-run@ is specified).
-- If you do not need the result of the IO action to construct the spec tree,
-- `Test.Hspec.Core.Hooks.beforeAll` may be more suitable for your use case.
runIO :: IO r -> SpecM a r
runIO = SpecM . liftIO

mapSpecForest :: ([SpecTree a] -> [SpecTree b]) -> SpecM a r -> SpecM b r
mapSpecForest f (SpecM specs) = SpecM (mapWriterT (fmap (fmap (second f))) specs)

-- {-# DEPRECATED mapSpecItem "Use `mapSpecItem_` instead." #-}
-- | Deprecated: Use `mapSpecItem_` instead.
mapSpecItem :: (ActionWith a -> ActionWith b) -> (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem _ = mapSpecItem_

mapSpecItem_ :: (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem_ = mapSpecForest . bimapForest id

modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
modifyParams f = mapSpecItem_ $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

data Env = Env {
  envSpecDescriptionPath :: [String]
, envAnnotations :: Annotations
}

withEnv :: (Env -> Env) -> SpecM a r -> SpecM a r
withEnv f = SpecM . WriterT . local f . runWriterT . unSpecM

withValue :: Typeable v => v -> SpecWith a -> SpecWith a
withValue v = withEnv $ modifyVault (Annotations.setValue v)
  where
    modifyVault :: (Annotations -> Annotations) -> Env -> Env
    modifyVault f env = env { envAnnotations = f (envAnnotations env) }

asks :: (Env -> r) -> SpecM a r
asks = SpecM . lift . Reader.asks

askValue :: Typeable v => SpecM a (Maybe v)
askValue = Annotations.getValue <$> asks envAnnotations
