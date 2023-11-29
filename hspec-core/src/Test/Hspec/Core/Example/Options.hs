{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Hspec.Core.Example.Options (
  Options(..)
, OptionsParser(..)
, Option(..)

, options
, option
, withRead

, liftOptions

, OptionsSet
, getOptions
, setOptions
, modifyOptions
, toDeclarativeOption
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable
import qualified GetOpt.Declarative as Declarative

data Option a = Option {
  optionShort :: Maybe Char
, optionName :: String
, optionPlaceholder :: String
, optionParser :: String -> a -> Maybe a
, optionHelp :: String
}

data OptionsParser a = OptionsParser {
  optionsParserTitle :: String
, optionsParserFlags :: [Option a]
}

class Typeable a => Options a where
  defaultOptions :: a
  optionsParser :: Maybe (OptionsParser a)

instance Options () where
  defaultOptions = ()
  optionsParser = Nothing

options :: String -> [Option a] -> Maybe (OptionsParser a)
options title = Just . OptionsParser title

option :: Maybe Char -> String -> String -> (String -> a -> Maybe a) -> String -> Option a
option = Option

withRead :: Read a => (a -> b -> b) -> String -> b -> Maybe b
withRead setter input args = setter <$> readMaybe input <*> pure args

liftOptions :: Options a => OptionsParser a -> OptionsParser OptionsSet
liftOptions p = p {optionsParserFlags = map liftOption $ optionsParserFlags p}

liftOption :: Options a => Option a -> Option OptionsSet
liftOption opt = opt {optionParser = p}
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

toDeclarativeOption :: Option OptionsSet -> Declarative.Option OptionsSet
toDeclarativeOption opt = Declarative.Option {
  Declarative.optionName = optionName opt
, Declarative.optionShortcut = optionShort opt
, Declarative.optionSetter = Declarative.Arg (optionPlaceholder opt) (optionParser opt)
, Declarative.optionHelp = optionHelp opt
, Declarative.optionDocumented = True
}
