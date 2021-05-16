module Test.Hspec.Core.Config.Options (
  ConfigFile
, envVarName
, ignoreConfigFile
, parseOptions
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           System.Exit

import           Test.Hspec.Core.Config.Definition
import qualified GetOpt.Declarative as Declarative
import           GetOpt.Declarative.Interpret (parse, interpretOptions, ParseResult(..))

type ConfigFile = (FilePath, [String])
type EnvVar = [String]

envVarName :: String
envVarName = "HSPEC_OPTIONS"

commandLineOptions :: [(String, [Declarative.Option Config])]
commandLineOptions =
    ("OPTIONS", commandLineOnlyOptions)
  : otherOptions

otherOptions :: [(String, [Declarative.Option Config])]
otherOptions = [
    ("RUNNER OPTIONS", runnerOptions)
  , ("FORMATTER OPTIONS", formatterOptions)
  , ("OPTIONS FOR QUICKCHECK", quickCheckOptions)
  , ("OPTIONS FOR SMALLCHECK", smallCheckOptions)
  ]

ignoreConfigFile :: Config -> [String] -> IO Bool
ignoreConfigFile config args = do
  ignore <- lookupEnv "IGNORE_DOT_HSPEC"
  case ignore of
    Just _ -> return True
    Nothing -> case parseCommandLineOptions "" args config of
      Right c -> return (configIgnoreConfigFile c)
      _ -> return False

parseOptions :: Config -> String -> [ConfigFile] -> Maybe EnvVar -> [String] -> Either (ExitCode, String) Config
parseOptions config prog configFiles envVar args = do
      foldM (parseFileOptions prog) config configFiles
  >>= maybe return (parseEnvVarOptions prog) envVar
  >>= parseCommandLineOptions prog args

parseCommandLineOptions :: String -> [String] -> Config -> Either (ExitCode, String) Config
parseCommandLineOptions prog args config = case Declarative.parseCommandLineOptions commandLineOptions prog args config of
  Success c -> Right c
  Help message -> Left (ExitSuccess, message)
  Failure message -> Left (ExitFailure 1, message)

parseFileOptions :: String -> Config -> ConfigFile -> Either (ExitCode, String) Config
parseFileOptions prog config (name, args) =
  parseOtherOptions prog ("in config file " ++ name) args config

parseEnvVarOptions :: String -> EnvVar -> Config -> Either (ExitCode, String) Config
parseEnvVarOptions prog =
  parseOtherOptions prog ("from environment variable " ++ envVarName)

parseOtherOptions :: String -> String -> [String] -> Config -> Either (ExitCode, String) Config
parseOtherOptions prog source args config = case parse (interpretOptions options) config args of
  Right c -> Right c
  Left err -> failure err
  where
    options :: [Declarative.Option Config]
    options = filter Declarative.optionDocumented $ concatMap snd otherOptions

    failure err = Left (ExitFailure 1, prog ++ ": " ++ message)
      where
        message = unlines $ case lines err of
          [x] -> [x ++ " " ++ source]
          xs -> xs ++ [source]
