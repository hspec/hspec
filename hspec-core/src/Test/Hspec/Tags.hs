{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Tags (
  use
, tag
, exclude

#ifdef TEST
, tagsOption
, Filter(..)
, getTagFilters
, parseTagFilters
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Data.String

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map (Map)
import qualified Data.Map as Map

import           Test.Hspec.Core.Extension

newtype Tag = Tag { unTag :: String }
  deriving (Eq, Show, Ord)

instance IsString Tag where
  fromString = Tag

getItemTags :: Item a -> Set Tag
getItemTags = fromMaybe mempty . getItemAnnotation

setItemTags :: Set Tag -> Item a -> Item a
setItemTags = setItemAnnotation

modifyItemTags :: (Set Tag -> Set Tag) -> Item a -> Item a
modifyItemTags f item =  setItemTags (f $ getItemTags item) item

addItemTag :: Tag -> Item a -> Item a
addItemTag = modifyItemTags . Set.insert

-- |
-- Tag all spec items of the given subtree.
--
-- It is idiomatic to use this in combination with `>>>`:
--
-- @
-- import Control.Arrow ((>>>))
-- import Test.Hspec
-- import Test.Hspec.Tags (tag)
--
-- spec :: Spec
-- spec = do
--   describe ".." $ do
--     it ".." >>> tag "slow" $ do
--       ..
-- @
tag :: String -> SpecWith a -> SpecWith a
tag = mapSpecItem . addItemTag . Tag

type TagFilters = Map Tag Filter

data Filter = Select | Discard | SetPending String
  deriving (Eq, Show)

getTagFilters :: Config -> TagFilters
getTagFilters = fromMaybe mempty . getConfigAnnotation

setTagFilters :: TagFilters -> Config -> Config
setTagFilters = setConfigAnnotation

modifyTagFilters :: (TagFilters -> TagFilters) -> Config -> Config
modifyTagFilters f config =  setTagFilters (f $ getTagFilters config) config

addTagFilter :: Tag -> Filter -> SpecWith a
addTagFilter name = modifyConfig . modifyTagFilters . Map.insert name

exclude :: String -> SpecWith a
exclude name = addTagFilter (Tag name) . SetPending $ unlines [
    "This item is tagged with " <> show name <> " and excluded by default."
  , "Use `--tags " <> name <> "' to include this item."
  ]

use :: SpecWith a
use = do
  registerOption "hspec-tags" tagsOption
  addTransformation filterByTags

filterByTags :: Config -> [SpecTree ()] -> [SpecTree ()]
filterByTags config = filterItems byTag . mapItems setPendingByTag
  where
    setPendingByTag :: Item () -> Item ()
    setPendingByTag item = foldl' (flip pendingBy) item pendingByTag
      where
        pendingBy :: (Tag, String) -> Item a -> Item a
        pendingBy (name, reason)
          | name `elem` tags = setItemPending (Just reason)
          | otherwise  = id

        tags :: Set Tag
        tags = getItemTags item

    byTag :: Item a -> Bool
    byTag = (||) <$> itemIsFocused <*> includeItem

    includeItem :: Item a -> Bool
    includeItem item = and $ map include filterByTag
      where
        include :: (Tag, Filter) -> Bool
        include (name, value) = case value of
          Select -> name `elem` tags
          Discard -> name `notElem` tags
          SetPending _ -> True

        tags :: Set Tag
        tags = getItemTags item

    pendingByTag :: [(Tag, String)]
    pendingByTag = [(name, reason) | (name, SetPending reason) <- filterByTag]

    filterByTag :: [(Tag, Filter)]
    filterByTag = Map.toList $ getTagFilters config

tagsOption :: Option
tagsOption = option "tags" (argument "TAGS" return addTagFilters) "TAGS can be a list of tag names."
  where
    addTagFilters :: String -> Config -> Config
    addTagFilters input = modifyTagFilters $ \ start -> (foldl' (\ tags tag_ -> insertTag tag_ tags) start (parseTagFilters input))

    insertTag :: (Tag, Maybe Filter) -> TagFilters -> TagFilters
    insertTag (name, new) = Map.alter f name
      where
        f :: Maybe Filter -> Maybe Filter
        f _ = new

parseTagFilters :: String -> [(Tag, Maybe Filter)]
parseTagFilters = map parse . words
  where
    parse :: String -> (Tag, Maybe Filter)
    parse input = case input of
      '-' : name -> (Tag name, Just Discard)
      '+' : name -> (Tag name, Nothing)
      name -> (Tag name, Just Select)
