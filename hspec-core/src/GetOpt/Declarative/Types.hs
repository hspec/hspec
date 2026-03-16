{-# LANGUAGE LambdaCase #-}
module GetOpt.Declarative.Types where

import           Prelude ()
import           Test.Hspec.Core.Compat

data Option config = Option {
  optionName :: String
, optionShortcut :: Maybe Char
, optionSetter :: OptionSetter config
, optionHelp :: String
, optionDocumented :: Bool
}

data OptionSetter config =
    NoArg (config -> config)
  | Flag (Bool -> config -> config)
  | OptArg String (Maybe String -> config -> Maybe config)
  | Arg String (String -> config -> Maybe config)

mapOption :: (b -> a) -> (a -> b) -> Option a -> Option b
mapOption from to option = option {
  optionSetter = mapOptionSetter from to $ optionSetter option
}

mapOptionSetter :: (b -> a) -> (a -> b) -> OptionSetter a -> OptionSetter b
mapOptionSetter from to = \ case
  NoArg f -> NoArg $ lift f
  Flag f -> Flag $ lift . f
  OptArg name f -> OptArg name $ liftF . f
  Arg name f -> Arg name $ liftF . f
  where
    lift f = to . f . from
    liftF f = fmap to . f . from
