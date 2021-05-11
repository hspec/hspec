module BumpVersion (main, Step(..), bumpVersion) where
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.List
import           Data.Version
import           Text.ParserCombinators.ReadP
import           System.Environment (getArgs)
import           System.Exit

name :: String
name = "version.yaml"

prefix :: String
prefix = "&version "

main :: IO ()
main = do
  args <- getArgs
  step <- case args of
    ["major"] -> return Major
    [] -> return Minor
    _ -> die $ "invalid arguments: " ++ show args
  T.readFile name >>= writeFile name . bumpVersion step . T.unpack

data Step = Major | Minor

bumpVersion :: Step -> String -> String
bumpVersion step = unlines . map go . lines
  where
    go :: String -> String
    go input = maybe input (render . bump step) $ parse input

bump :: Step -> Version -> Version
bump step v = case step of
  Major -> makeVersion [x, succ y, 0]
  Minor -> makeVersion [x, y, succ z]
  where
    x : y : z : _ = versionBranch v

render :: Version -> String
render = mappend prefix . showVersion

parse :: String -> Maybe Version
parse = stripPrefix prefix >=> runReadP parseVersion

runReadP :: ReadP a -> String -> Maybe a
runReadP p input = case reverse $ readP_to_S p input of
  (v, "") : _ -> Just v
  _ -> Nothing
