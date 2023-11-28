module Test.Hspec.Core.Extension (
  Option
, flag
, option
, argument
, registerOption

, Config.getConfigAnnotation
, Config.setConfigAnnotation

, getItemAnnotation
, setItemAnnotation

, addTransformation

, Config
, modifyConfig


, SpecWith
, runIO

, mapSpecItem
, mapItems
, setItemPending

, Item(itemIsFocused)
, SpecTree

, filterItems
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified GetOpt.Declarative as Config
import           Test.Hspec.Core.Spec hiding (filterForest, mapSpecItem)
import           Test.Hspec.Core.Tree hiding (filterForest)
import qualified Test.Hspec.Core.Tree as Tree
import           Test.Hspec.Core.Config.Definition (Config)
import qualified Test.Hspec.Core.Config.Definition as Config

type Option = Config.Option Config
type OptionSetter = Config.OptionSetter Config

flag :: String -> (Bool -> Config -> Config) -> String -> Option
flag name setter = Config.flag name setter

option :: String -> OptionSetter -> String -> Option
option = Config.option

argument :: String -> (String -> Maybe a) -> (a -> Config -> Config) -> OptionSetter
argument = Config.argument

registerOption :: String -> Option -> SpecWith a
registerOption section = modifyConfig . Config.addCustomOption section

addTransformation :: (Config -> [SpecTree ()] -> [SpecTree ()]) -> SpecWith a
addTransformation = modifyConfig . Config.addSpecTransformation

mapSpecItem :: (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem = mapSpecItem_

mapItems :: (Item () -> Item ()) -> [SpecTree ()] -> [SpecTree ()]
mapItems = bimapForest id

filterItems :: (Item () -> Bool) -> [SpecTree ()] -> [SpecTree ()]
filterItems = Tree.filterForest

setItemPending :: Maybe String -> Item a -> Item a
setItemPending reason item = item { itemExample = \ _params _hook _progress -> result }
  where
    result :: IO Result
    result = return $ Result "" (Pending Nothing reason)
