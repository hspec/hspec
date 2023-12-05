{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

liftOption :: (config -> a) -> (a -> config -> config) -> Option a -> Option config
liftOption get set option = option {
  optionSetter = liftOptionSetter get set $ optionSetter option
}

liftOptionSetter :: forall a config. (config -> a) -> (a -> config -> config) -> OptionSetter a -> OptionSetter config
liftOptionSetter get set = \ case
  NoArg f -> NoArg $ modifyWith get set f
  Flag f -> Flag $ modifyWith get set . f
  OptArg name f -> OptArg name $ modifyMaybe . f
  Arg name f -> Arg name $ modifyMaybe . f
  where
    modifyMaybe :: (a -> Maybe a) -> config -> Maybe config
    modifyMaybe modify config = flip set config <$> modify (get config)
