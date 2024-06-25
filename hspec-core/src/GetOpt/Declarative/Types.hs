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
