{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Config.Options (
  ConfigFile
, envVarName
, ignoreConfigFile
, parseOptions
, customOptionsFoo
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           System.Exit

import           Test.Hspec.Core.Format (Format, FormatConfig)
import           Test.Hspec.Core.Config.Definition
import           Test.Hspec.Core.Example.Options
import qualified GetOpt.Declarative as Declarative
import           GetOpt.Declarative.Interpret (parse, interpretOptions, ParseResult(..))

type ConfigFile = (FilePath, [String])
type EnvVar = [String]

envVarName :: String
envVarName = "HSPEC_OPTIONS"

commandLineOptions :: [(String, FormatConfig -> IO Format)] -> [(String, [Declarative.Option Config])] -> [(String, [Declarative.Option Config])]
commandLineOptions formatters extensionOptions =
    ("OPTIONS", commandLineOnlyOptions)
  : otherOptions formatters extensionOptions

otherOptions :: [(String, FormatConfig -> IO Format)] -> [(String, [Declarative.Option Config])] -> [(String, [Declarative.Option Config])]
otherOptions formatters extensionOptions = [
    ("RUNNER OPTIONS", runnerOptions)
  , ("FORMATTER OPTIONS", formatterOptions formatters)
  , ("OPTIONS FOR QUICKCHECK", quickCheckOptions)
  , ("OPTIONS FOR SMALLCHECK", smallCheckOptions)
  ] ++ extensionOptions

customOptionsFoo :: [OptionsParser OptionsSet] -> [(String, [Declarative.Option Config])]
customOptionsFoo = map bar

bar :: OptionsParser OptionsSet -> (String, [Declarative.Option Config])
bar (OptionsParser title flags) = (title, map baz flags)

baz :: Flag OptionsSet -> Declarative.Option Config
baz flag__ = Declarative.Option {
  Declarative.optionName = optionName flag__
, Declarative.optionShortcut = optionShort flag__
, Declarative.optionSetter = qux (optionPlaceholder flag__) (optionParser flag__)
, Declarative.optionHelp = optionHelp flag__
, Declarative.optionDocumented = True
}

qux :: String -> (String -> OptionsSet -> Maybe OptionsSet) -> Declarative.OptionSetter Config
qux placeholder = Declarative.Arg placeholder . f
  where
    f :: (String -> OptionsSet -> Maybe OptionsSet) -> String -> Config -> Maybe Config
    f parser input config = case parser input (configValues config) of
      Nothing -> Nothing
      Just new -> Just config { configValues = new }

ignoreConfigFile :: Config -> [String] -> IO Bool
ignoreConfigFile config args = do
  ignore <- lookupEnv "IGNORE_DOT_HSPEC"
  case ignore of
    Just _ -> return True
    Nothing -> case parseCommandLineOptions "" args config of
      Right c -> return (configIgnoreConfigFile c)
      _ -> return False

parseOptions :: Config -> String -> [ConfigFile] -> Maybe EnvVar -> [(String, String)] -> [String] -> Either (ExitCode, String) ([String], Config)
parseOptions config prog configFiles envVar env args = do
      foldM (parseFileOptions prog) config configFiles
  >>= maybe return (parseEnvVarOptions prog) envVar
  >>= parseEnvironmentOptions env
  >>= traverse (parseCommandLineOptions prog args)

parseCommandLineOptions :: String -> [String] -> Config -> Either (ExitCode, String) Config
parseCommandLineOptions prog args config = case Declarative.parseCommandLineOptions (commandLineOptions formatters extensionOptions) prog args config of
  Success c -> Right c
  Help message -> Left (ExitSuccess, message)
  Failure message -> Left (ExitFailure 1, message)
  where
    formatters = configAvailableFormatters config
    extensionOptions = configCustomOptions config

parseEnvironmentOptions :: [(String, String)] -> Config -> Either (ExitCode, String) ([String], Config)
parseEnvironmentOptions env config = case Declarative.parseEnvironmentOptions "HSPEC" env config (concatMap snd $ commandLineOptions formatters extensionOptions) of
  (warnings, c) -> Right (map formatWarning warnings, c)
  where
    formatters = configAvailableFormatters config
    extensionOptions = configCustomOptions config
    formatWarning (Declarative.InvalidValue name value) = "invalid value `" ++ value ++ "' for environment variable " ++ name

parseFileOptions :: String -> Config -> ConfigFile -> Either (ExitCode, String) Config
parseFileOptions prog config (name, args) =
  parseOtherOptions prog ("in config file " ++ name) args config

parseEnvVarOptions :: String -> EnvVar -> Config -> Either (ExitCode, String) Config
parseEnvVarOptions prog =
  parseOtherOptions prog ("from environment variable " ++ envVarName)

parseOtherOptions :: String -> String -> [String] -> Config -> Either (ExitCode, String) Config
parseOtherOptions prog source args config = case parse (interpretOptions options__) config args of
  Right c -> Right c
  Left err -> failure err
  where
    options__ :: [Declarative.Option Config]
    options__ = filter Declarative.optionDocumented $ concatMap snd (otherOptions formatters extensionOptions)

    formatters = configAvailableFormatters config
    extensionOptions = configCustomOptions config

    failure err = Left (ExitFailure 1, prog ++ ": " ++ message)
      where
        message = unlines $ case lines err of
          [x] -> [x ++ " " ++ source]
          xs -> xs ++ [source]
