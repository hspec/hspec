module Config (
  Config (..)
, defaultConfig
, parseConfig
, usage
) where

import           Data.Maybe
import           System.Console.GetOpt

data Config = Config {
  configNested :: Bool
, configFormatter :: Maybe String
, configNoMain :: Bool
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config False Nothing False

options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["nested"]    (NoArg  $ \c -> c   {configNested = True}) ""
  , Option [] ["formatter"] (ReqArg (\s c -> c {configFormatter = Just s}) "FORMATTER") ""
  , Option [] ["no-main"]   (NoArg  $ \c -> c   {configNoMain = True}) ""
  ]

usage :: String -> String
usage prog = "\nUsage: " ++ prog ++ " SRC CUR DST [--formatter=FORMATTER] [--no-main]\n"

parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> let
        c = (foldl (flip id) defaultConfig opts)
      in
        if (configNoMain c && isJust (configFormatter c))
           then
             formatError "option `--formatter=<fmt>' does not make sense with `--no-main'\n"
           else
             Right c
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "'\n")
  where
    formatError err = Left (prog ++ ": " ++ err ++ usage prog)
