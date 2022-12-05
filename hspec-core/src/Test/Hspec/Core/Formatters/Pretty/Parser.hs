module Test.Hspec.Core.Formatters.Pretty.Parser (
  Value(..)
, parseValue
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Formatters.Pretty.Parser.Parser hiding (Parser)
import qualified Test.Hspec.Core.Formatters.Pretty.Parser.Parser as P

import           Language.Haskell.Lexer hiding (Pos(..))

type Name = String

data Value =
    Char Char
  | String String
  | Rational Value Value
  | Number String
  | Record Name [(Name, Value)]
  | Constructor Name [Value]
  | Tuple [Value]
  | List [Value]
  deriving (Eq, Show)

type Parser = P.Parser (Token, String)

parseValue :: String -> Maybe Value
parseValue input = case runParser value (tokenize input) of
  Just (v, []) -> Just v
  _ -> Nothing

value :: Parser Value
value =
      char
  <|> string
  <|> rational
  <|> number
  <|> record
  <|> constructor
  <|> tuple
  <|> list

char :: Parser Value
char = Char <$> (token CharLit >>= readA)

string :: Parser Value
string = String <$> (token StringLit >>= readA)

rational :: Parser Value
rational = Rational <$> (number <|> tuple) <* require (Varsym, "%") <*> number

number :: Parser Value
number = integer <|> float
  where
    integer :: Parser Value
    integer = Number <$> token IntLit

    float :: Parser Value
    float = Number <$> token FloatLit

record :: Parser Value
record = Record <$> token Conid <* special "{" <*> fields <* special "}"
  where
    fields :: Parser [(Name, Value)]
    fields = field `sepBy1` comma

    field :: Parser (Name, Value)
    field = (,) <$> token Varid <* equals <*> value

constructor :: Parser Value
constructor = Constructor <$> token Conid <*> many value

tuple :: Parser Value
tuple = Tuple <$> (special "(" *> items) <* special ")"

list :: Parser Value
list = List <$> (special "[" *> items) <* special "]"

items :: Parser [Value]
items = value `sepBy` comma

special :: String -> Parser ()
special s = require (Special, s)

comma :: Parser ()
comma = special ","

equals :: Parser ()
equals = require (Reservedop, "=")

token :: Token -> Parser String
token t = snd <$> satisfy (fst >>> (== t))

require :: (Token, String) -> Parser ()
require t = void $ satisfy (== t)

tokenize :: String -> [(Token, String)]
tokenize = go . map (fmap snd) . rmSpace . lexerPass0
  where
    go :: [(Token, String)] -> [(Token, String)]
    go tokens = case tokens of
      [] -> []
      (Varsym, "-") : (IntLit, n) : xs -> (IntLit, "-" ++ n) : go xs
      (Varsym, "-") : (FloatLit, n) : xs -> (FloatLit, "-" ++ n) : go xs
      x : xs -> x : go xs
