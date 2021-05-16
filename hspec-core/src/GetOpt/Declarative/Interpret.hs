module GetOpt.Declarative.Interpret (
  ParseResult(..)
, parseCommandLineOptions
, parse
, interpretOptions
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Data.Maybe

import           System.Console.GetOpt (OptDescr, ArgOrder(..), getOpt)
import qualified System.Console.GetOpt as GetOpt

import           GetOpt.Declarative.Types
import           GetOpt.Declarative.Util (mkUsageInfo, mapOptDescr)

data InvalidArgument = InvalidArgument String String

data ParseResult config = Help String | Failure String | Success config

parseCommandLineOptions :: [(String, [Option config])] -> String -> [String] -> config -> ParseResult config
parseCommandLineOptions opts prog args config = case parseWithHelp (concatMap snd options) config args of
  Nothing -> Help usage
  Just (Right c) -> Success c
  Just (Left err) -> Failure $ prog ++ ": " ++ err ++ "\nTry `" ++ prog ++ " --help' for more information.\n"
  where
    options = addHelpFlag $ map (fmap interpretOptions) opts

    documentedOptions = addHelpFlag $ map (fmap $ interpretOptions . filter optionDocumented) opts

    usage :: String
    usage = "Usage: " ++ prog ++ " [OPTION]...\n\n"
      ++ (intercalate "\n" $ map (uncurry mkUsageInfo) documentedOptions)

addHelpFlag :: [(a, [OptDescr a1])] -> [(a, [OptDescr (Maybe a1)])]
addHelpFlag opts = case opts of
  (section, xs) : ys -> (section, GetOpt.Option [] ["help"] (GetOpt.NoArg help) "display this help and exit" : noHelp xs) : map (fmap noHelp) ys
  [] -> []
  where
    help = Nothing

    noHelp :: [OptDescr a] -> [OptDescr (Maybe a)]
    noHelp = map (mapOptDescr Just)

parseWithHelp :: [OptDescr (Maybe (config -> Either InvalidArgument config))] -> config -> [String] -> Maybe (Either String config)
parseWithHelp options config args = case getOpt Permute options args of
  (opts, [], []) | _ : _ <- [() | Nothing <- opts] -> Nothing
  (opts, xs, ys) -> Just $ interpretResult config (catMaybes opts, xs, ys)

parse :: [OptDescr (config -> Either InvalidArgument config)] -> config -> [String] -> Either String config
parse options config = interpretResult config . getOpt Permute options

interpretResult :: config -> ([config -> Either InvalidArgument config], [String], [String]) -> Either String config
interpretResult config = interpretGetOptResult >=> foldResult config

foldResult :: config -> [config -> Either InvalidArgument config] -> Either String config
foldResult config opts = either (Left . renderInvalidArgument) return $ foldlM (flip id) config opts

renderInvalidArgument :: InvalidArgument -> String
renderInvalidArgument (InvalidArgument name value) = "invalid argument `" ++ value ++ "' for `--" ++ name ++ "'"

interpretGetOptResult :: ([a], [String], [String]) -> Either String [a]
interpretGetOptResult result = case result of
  (opts, [], []) -> Right opts
  (_, _, err:_) -> Left (init err)
  (_, arg:_, _) -> Left ("unexpected argument `" ++ arg ++ "'")

interpretOptions :: [Option config] -> [OptDescr (config -> Either InvalidArgument config)]
interpretOptions = concatMap interpretOption

interpretOption :: Option config -> [OptDescr (config -> Either InvalidArgument config)]
interpretOption (Option name shortcut argDesc help _) = case argDesc of
  NoArg setter -> [option $ GetOpt.NoArg (Right . setter)]

  Flag setter -> [
      option (arg True)
    , GetOpt.Option [] ["no-" ++ name] (arg False) ("do not " ++ help)
    ]
    where
      arg v = GetOpt.NoArg (Right . setter v)

  OptArg argName setter -> [option $ GetOpt.OptArg arg argName]
    where
      arg mInput c = case setter mInput c of
        Just c_ -> Right c_
        Nothing -> case mInput of
          Just input -> invalid input
          Nothing -> Right c

  Arg argName setter -> [option (GetOpt.ReqArg arg argName)]
    where
      arg input = maybe (invalid input) Right . setter input

  where
    invalid = Left . InvalidArgument name
    option arg = GetOpt.Option (maybeToList shortcut) [name] arg help
