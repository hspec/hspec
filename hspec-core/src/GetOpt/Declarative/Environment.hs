module GetOpt.Declarative.Environment (
  InvalidValue(..)
, parseEnvironmentOptions
, parseEnvironmentOption
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Data.Char

import           GetOpt.Declarative.Types

data InvalidValue = InvalidValue String String
  deriving (Eq, Show)

parseEnvironmentOptions :: String -> [(String, String)] -> config -> [Option config] -> ([InvalidValue], config)
parseEnvironmentOptions prefix env = foldr f . (,) []
  where
    f :: Option config -> ([InvalidValue], config) -> ([InvalidValue], config)
    f option (errs, config) = case parseEnvironmentOption prefix env config option of
        Left err -> (err : errs, config)
        Right c -> (errs, c)

parseEnvironmentOption :: String -> [(String, String)] -> config -> Option config -> Either InvalidValue config
parseEnvironmentOption prefix env config option = case lookup name env of
  Nothing -> Right config
  Just value -> case optionSetter option of
    NoArg setter -> case value of
      "yes" -> Right $ setter config
      _ -> invalidValue
    Flag setter -> case value of
      "yes" -> Right $ setter True config
      "no" -> Right $ setter False config
      _ -> invalidValue
    OptArg _ setter -> case setter (Just value) config of
      Just c -> Right c
      Nothing -> invalidValue
    Arg _ setter -> case setter value config of
      Just c -> Right c
      Nothing -> invalidValue
    where
      invalidValue = Left (InvalidValue name value)
  where
    name = envVarName prefix option

envVarName :: String -> Option config -> String
envVarName prefix option = prefix ++ '_' : map f (optionName option)
  where
    f c = case c of
      '-' -> '_'
      _ -> toUpper c
