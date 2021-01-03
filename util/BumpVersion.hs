module BumpVersion (main, bumpVersion) where
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.List
import           Data.Version
import           Text.ParserCombinators.ReadP

name :: String
name = "version.yaml"

prefix :: String
prefix = "&version "

main :: IO ()
main = T.readFile name >>= writeFile name . bumpVersion . T.unpack

bumpVersion :: String -> String
bumpVersion = unlines . map go . lines
  where
    go :: String -> String
    go input = maybe input (render . bump) $ parse input

bump :: Version -> Version
bump v = makeVersion [x, y, succ z]
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
