module Config (
  Config (..)
, defaultConfig
, helpMessage
, parseConfig
, usage
) where

import           Data.Maybe
import           System.Console.GetOpt

data Config = Config {
  configNested :: Bool
, configFormatter :: Maybe String
, configNoMain :: Bool
, configModuleName :: Maybe String
, configHelp :: Bool
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config False Nothing False Nothing False

options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["nested"] (NoArg $ \c -> c {configNested = True}) ""
  , Option [] ["formatter"] (ReqArg (\s c -> c {configFormatter = Just s}) "FORMATTER") ""
  , Option [] ["module-name"] (ReqArg (\s c -> c {configModuleName = Just s}) "NAME") ""
  , Option [] ["no-main"] (NoArg $ \c   -> c {configNoMain = True}) ""
  , Option [] ["help"] (NoArg $ \c -> c {configHelp = True}) ""
  ]

usage :: String -> String
usage prog = "Usage: " ++ prog ++ " SRC CUR DST [--module-name=NAME]"

parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> let
        c = foldl (flip id) defaultConfig opts
      in validateConfig c
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "'\n")
  where
    formatError err = Left (prog ++ ": " ++ err ++ "\n" ++ usage prog ++ "\n")

    validateConfig c | configNoMain c && isJust (configFormatter c) =
        formatError "option `--formatter=<fmt>' does not make sense with `--no-main'\n"
    validateConfig c = Right c

helpMessage :: String -> String
helpMessage prog = usageInfo (usage prog) options
