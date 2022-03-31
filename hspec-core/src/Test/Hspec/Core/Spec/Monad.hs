{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- NOTE: re-exported from Test.Hspec.Core.Spec
module Test.Hspec.Core.Spec.Monad (
  Spec
, SpecWith
, SpecM (..)
, runSpecM
, fromSpecList
, runIO

, mapSpecForest
, mapSpecForest_
, mapSpecItem
, mapSpecItem_
, modifyParams

, modifyConfig

, Env(..)
, askAncestors
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Arrow
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree

import           Test.Hspec.Core.Config.Definition (Config)
import Control.Monad.Trans.Reader (ReaderT (runReaderT, ReaderT), withReaderT, mapReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans(lift))

type Spec = SpecWith ()

type SpecWith a = SpecM a ()

-- |
-- @since 2.10.0
modifyConfig :: (Config -> Config) -> SpecWith a
modifyConfig f = SpecM $ tell (Endo f, mempty)

-- | A writer monad for `SpecTree` forests
newtype SpecM a r = SpecM (WriterT (Endo Config, [SpecTree a]) (ReaderT Env IO) r)
  deriving (Functor, Applicative, Monad)

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: SpecWith a -> IO (Endo Config, [SpecTree a])
runSpecM (SpecM specs) = flip runReaderT (Env []) . execWriterT $ specs

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
mapSpecForest = mapSpecForest_ id

mapSpecForest_ :: (Env -> Env) -> ([SpecTree a] -> [SpecTree b]) -> SpecM a r -> SpecM b r
mapSpecForest_ ef tf (SpecM specs) = SpecM $ flip mapWriterT specs $ 
  mapReaderT (fmap (fmap (second tf))) . withReaderT ef

mapSpecItem :: (ActionWith a -> ActionWith b) -> (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem g f = mapSpecForest (bimapForest g f)

mapSpecItem_ :: (Item a -> Item a) -> SpecWith a -> SpecWith a
mapSpecItem_ = mapSpecItem id

modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
modifyParams f = mapSpecItem_ $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

-- | Environment of a test definition, in the Spec monad
data Env = Env 
  { envAncestorGroups :: [String]  -- Should this be non empty?
  }
  deriving (Eq, Show, Ord)

askAncestors :: SpecM a [String]
askAncestors = do 
  SpecM $ lift $ asks envAncestorGroups