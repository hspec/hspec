{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Type (
  Spec
, SpecWith
, SpecM (..)
, runSpecM
, fromSpecList
, Tree (..)
, SpecTree
, mapSpecTree
, Item (..)
, ActionWith
, Location (..)
, LocationAccuracy(..)

, mapSpecItem
, mapSpecItem_

, specGroup
, specItem

, runIO
) where

import           Control.Applicative
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree

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
-- If you do not need the result of the IO action to construct the spec tree,
-- `Test.Hspec.beforeAll` may be more suitable for your use case.
runIO :: IO r -> SpecM a r
runIO = SpecM . liftIO

type SpecTree a = Tree (ActionWith a) (Item a)

data Item a = Item {
  itemRequirement :: String
, itemLocation :: Maybe Location
, itemIsParallelizable :: Bool
, itemExample :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
}

mapSpecTree :: (SpecTree a -> SpecTree b) -> SpecWith a -> SpecWith b
mapSpecTree f spec = runIO (runSpecM spec) >>= fromSpecList . map f

mapSpecItem :: (ActionWith a -> ActionWith b) -> (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem g f = mapSpecTree go
  where
    go spec = case spec of
      Node d xs -> Node d (map go xs)
      NodeWithCleanup cleanup xs -> NodeWithCleanup (g cleanup) (map go xs)
      Leaf item -> Leaf (f item)

mapSpecItem_ :: (Item a -> Item a) -> SpecWith a -> SpecWith a
mapSpecItem_ = mapSpecItem id

data LocationAccuracy = ExactLocation | BestEffort
  deriving (Eq, Show)

data Location = Location {
  locationFile :: String
, locationLine :: Int
, locationColumn :: Int
, locationAccuracy :: LocationAccuracy
} deriving (Eq, Show)

-- | The @specGroup@ function combines a list of specs into a larger spec.
specGroup :: String -> [SpecTree a] -> SpecTree a
specGroup s = Node msg
  where
    msg
      | null s = "(no description given)"
      | otherwise = s

-- | Create a spec item.
specItem :: Example a => String -> a -> SpecTree (Arg a)
specItem s e = Leaf $ Item requirement Nothing False (evaluateExample e)
  where
    requirement
      | null s = "(unspecified behavior)"
      | otherwise = s
