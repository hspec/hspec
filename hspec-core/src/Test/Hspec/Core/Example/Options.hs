{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Hspec.Core.Example.Options (
  Options(..)
, OptionsParser(..)
, Flag(..)

, options
, flag
, withRead

, toCustomOptions

, OptionsSet
, getOptions
, setOptions
, modifyOptions
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable

data Flag a = Flag {
  optionShort :: Maybe Char
, optionName :: String
, optionPlaceholder :: String
, optionParser :: String -> a -> Maybe a
, optionHelp :: String
}

data OptionsParser a = OptionsParser {
  optionsParserTitle :: String
, optionsParserFlags :: [Flag a]
}

class Typeable a => Options a where
  defaultOptions :: a
  optionsParser :: Maybe (OptionsParser a)

instance Options () where
  defaultOptions = ()
  optionsParser = Nothing

options :: String -> [Flag a] -> Maybe (OptionsParser a)
options title = Just . OptionsParser title

flag :: Maybe Char -> String -> String -> (String -> a -> Maybe a) -> String -> Flag a
flag = Flag

withRead :: Read a => (a -> b -> b) -> String -> b -> Maybe b
withRead setter input args = setter <$> readMaybe input <*> pure args

toCustomOptions :: Options a => OptionsParser a -> OptionsParser OptionsSet
toCustomOptions p = p {optionsParserFlags = map toCustomOptionsFlag $ optionsParserFlags p}

toCustomOptionsFlag :: Options a => Flag a -> Flag OptionsSet
toCustomOptionsFlag opt = opt {optionParser = p}
  where
    p input set = setOptions <$> optionParser opt input (getOptions set) <*> pure set

newtype OptionsSet = OptionsSet (Map TypeRep Value)
  deriving (
#if MIN_VERSION_base(4,9,0)
    Semigroup, 
#endif
    Monoid)

data Value = forall a. Options a => Value a

setOptions :: Options a => a -> OptionsSet -> OptionsSet
setOptions v (OptionsSet m) = OptionsSet (Map.insert key value m)
  where
    key = typeOf v
    value = Value v

getOptions :: Options a => OptionsSet -> a
getOptions (OptionsSet m) = case Map.lookup (typeOf def) m of
  Just (Value v) | Just value <- cast v -> value
  Just _ -> error "this should never happen"
  Nothing -> def
  where
    def = defaultOptions

modifyOptions :: Options a => (a -> a) -> OptionsSet -> OptionsSet
modifyOptions f m = setOptions (f $ getOptions m) m
