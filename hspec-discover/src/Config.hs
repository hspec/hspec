module Config (
  Config (..)
, defaultConfig
, parseConfig
, usage
) where

import           System.Console.GetOpt

data Config = Config {
  configNested :: Bool
, configFormatter :: Maybe String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config False Nothing

options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["nested"]    (NoArg  $ \c -> c   {configNested = True}) ""
  , Option [] ["formatter"] (ReqArg (\s c -> c {configFormatter = Just s}) "FORMATTER") ""
  ]

usage :: String -> String
usage prog = "\nUsage: " ++ prog ++ " SRC CUR DST [--nested] [--formatter=FORMATTER]\n"

parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> Right (foldl (flip id) defaultConfig opts)
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "'\n")
  where
    formatError err = Left (prog ++ ": " ++ err ++ usage prog)
