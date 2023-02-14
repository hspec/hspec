-- |
-- /NOTE:/ This module is not meant for public consumption.  For user
-- documentation look at https://hspec.github.io/hspec-discover.html.
module Test.Hspec.Discover.Config (
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
, configModuleName :: Maybe String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config False Nothing False Nothing

options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["nested"] (NoArg $ \c -> c {configNested = True}) ""
  , Option [] ["formatter"] (ReqArg (\s c -> c {configFormatter = Just s}) "FORMATTER") ""
  , Option [] ["module-name"] (ReqArg (\s c -> c {configModuleName = Just s}) "NAME") ""
  , Option [] ["no-main"] (NoArg $ \c   -> c {configNoMain = True}) ""
  ]

usage :: String -> String
usage prog = "\nUsage: " ++ prog ++ " SRC CUR DST [--module-name=NAME]\n"

parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> let
        c = foldl (flip id) defaultConfig opts
      in
        if configNoMain c && isJust (configFormatter c)
           then
             formatError "option `--formatter=<fmt>' does not make sense with `--no-main'\n"
           else
             Right c
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "'\n")
  where
    formatError err = Left (prog ++ ": " ++ err ++ usage prog)
