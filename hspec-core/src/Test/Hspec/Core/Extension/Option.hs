module Test.Hspec.Core.Extension.Option {-# WARNING "This API is experimental." #-} (
  Option
, flag
, argument
, noArgument
, optionalArgument
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified GetOpt.Declarative as Declarative
import qualified Test.Hspec.Core.Config.Definition as Core

import           Test.Hspec.Core.Extension.Config.Type (Option(..), Config)

flag :: String -> (Bool -> Config -> Config) -> String -> Option
flag name setter = Option . Core.flag name setter

argument :: String -> String -> (String -> Config -> Maybe Config) -> String -> Option
argument name argumentName setter = Option . Core.option name (Declarative.Arg argumentName setter)

optionalArgument :: String -> String -> (Maybe String -> Config -> Maybe Config) -> String -> Option
optionalArgument name argumentName setter = Option . Core.option name (Declarative.OptArg argumentName setter)

noArgument :: String -> (Config -> Config) -> String -> Option
noArgument name setter help = Option $ Declarative.Option name Nothing (Declarative.NoArg setter) help False
