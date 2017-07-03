{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Stability: unstable
module Test.Hspec.Core.Tree (
  SpecTree
, Tree (..)
, Item (..)
, specGroup
, specItem
, location
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.CallStack

import           Test.Hspec.Core.Example

-- | Internal tree data structure
data Tree c a =
    Node String [Tree c a]
  | NodeWithCleanup c [Tree c a]
  | Leaf a
  deriving (Functor, Foldable, Traversable)

-- | A tree is used to represent a spec internally.  The tree is parametrize
-- over the type of cleanup actions and the type of the actual spec items.
type SpecTree a = Tree (ActionWith a) (Item a)

-- |
-- @Item@ is used to represent spec items internally.  A spec item consists of:
--
-- * a textual description of a desired behavior
-- * an example for that behavior
-- * additional meta information
--
-- Everything that is an instance of the `Example` type class can be used as an
-- example, including QuickCheck properties, Hspec expectations and HUnit
-- assertions.
data Item a = Item {
  -- | Textual description of behavior
  itemRequirement :: String
  -- | Source location of the spec item
, itemLocation :: Maybe Location
  -- | A flag that indicates whether it is safe to evaluate this spec item in
  -- parallel with other spec items
, itemIsParallelizable :: Maybe Bool
  -- | Example for behavior
, itemExample :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
}

-- | The @specGroup@ function combines a list of specs into a larger spec.
specGroup :: String -> [SpecTree a] -> SpecTree a
specGroup s = Node msg
  where
    msg
      | null s = "(no description given)"
      | otherwise = s

-- | The @specItem@ function creates a spec item.
specItem :: (HasCallStack, Example a) => String -> a -> SpecTree (Arg a)
specItem s e = Leaf $ Item requirement location Nothing (safeEvaluateExample e)
  where
    requirement
      | null s = "(unspecified behavior)"
      | otherwise = s

location :: HasCallStack => Maybe Location
location = case reverse callStack of
  (_, loc) : _ -> Just (Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc))
  _ -> Nothing
