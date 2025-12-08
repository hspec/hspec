{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Formatters.Pretty.Parser (
  Value(..)
, parseValue
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (Alternative(..), read, exp)

import           Language.Haskell.Lexer hiding (Token, Pos(..))
import qualified Language.Haskell.Lexer as Lexer

type Name = String

data Value =
    Char Char
  | String String
  | Number String
  | Operator Value Name Value
  | Record Name [(Name, Value)]
  | Constructor Name [Value]
  | Tuple [Value]
  | List [Value]
  deriving (Eq, Show)

type Token = (TokenType, String)

type TokenType = Lexer.Token

#ifndef __MHS__
newtype Parser a = Parser {
#else
data Parser a = Parser {
#endif
  runParser :: [Token] -> Maybe (a, [Token])
} deriving Functor

instance Applicative Parser where
  pure a = Parser $ \ input -> Just (a, input)
  (<*>) = ap

instance Monad Parser where
  return = pure
  p1 >>= p2 = Parser $ runParser p1 >=> uncurry (runParser . p2)

parseValue :: String -> Maybe Value
parseValue input = case runParser exp (tokenize input) of
  Just (v, []) -> Just v
  _ -> Nothing

tokenize :: String -> [Token]
tokenize = go . map (fmap snd) . rmSpace . lexerPass0
  where
    go :: [Token] -> [Token]
    go tokens = case tokens of
      [] -> []
      (Varsym, "-") : (IntLit, n) : xs -> (IntLit, "-" ++ n) : go xs
      (Varsym, "-") : (FloatLit, n) : xs -> (FloatLit, "-" ++ n) : go xs
      x : xs -> x : go xs

exp :: Parser Value
exp = infixexp

infixexp :: Parser Value
infixexp = aexp accept reject
  where
    accept :: Value -> Parser Value
    accept value = peek >>= \ case
      (Varsym, "%") -> skip >> Operator value "%" <$> exp
      (Consym, name) -> skip >> Operator value name <$> exp
      _ -> return value

    reject :: Parser Value
    reject = next >>= unexpected

aexp :: forall a. (Value -> Parser a) -> Parser a -> Parser a
aexp continue done = peek >>= \ case
  (CharLit, value) -> read Char value
  (StringLit, value) -> read String value
  (IntLit, value) -> number value
  (FloatLit, value) -> number value
  (Conid, name) -> accept $ constructor name
  (Special, "(") -> accept tuple
  (Special, "[") -> accept list
  _ -> done
  where
    read :: Read v => (v -> Value) -> String -> Parser a
    read c v = skip >> c <$> readValue v >>= continue

    number :: String -> Parser a
    number value = skip >> continue (Number value)

    accept :: Parser Value -> Parser a
    accept action = skip >> action >>= continue

constructor :: String -> Parser Value
constructor name = peek >>= \ case
  (Special, "{") -> skip >> Record name <$> commaSeparated field "}"
    where
      field :: Parser (Name, Value)
      field = (,) <$> require Varid <* equals <*> exp

  _ -> Constructor name <$> parameters
    where
      parameters :: Parser [Value]
      parameters = aexp more done

      more :: Value -> Parser [Value]
      more v = (:) v <$> parameters

      done :: Parser [Value]
      done = return []

tuple :: Parser Value
tuple = Tuple <$> commaSeparated exp ")"

list :: Parser Value
list = List <$> commaSeparated exp "]"

commaSeparated :: forall a. Parser a -> String -> Parser [a]
commaSeparated item close = peek >>= \ case
  (Special, c) | c == close -> skip >> return []
  _ -> items
  where
    items :: Parser [a]
    items = (:) <$> item <*> moreItems

    moreItems :: Parser [a]
    moreItems = next >>= \ case
      (Special, c)
        | c == "," -> items
        | c == close -> return []
      t -> unexpected t

equals :: Parser ()
equals = requireToken (Reservedop, "=")

require :: TokenType -> Parser String
require expected = next >>= \ case
  (token, v) | token == expected -> return v
  token -> unexpected token

requireToken :: Token -> Parser ()
requireToken expected = next >>= \ case
  token | token == expected -> return ()
  token -> unexpected token

peek :: Parser Token
peek = Parser $ \ input -> case input of
  t : _ -> Just (t, input)
  [] -> Just ((GotEOF, ""), input)

next :: Parser Token
next = Parser $ \ case
  t : ts -> Just (t, ts)
  ts -> Just ((GotEOF, ""), ts)

skip :: Parser ()
skip = Parser $ \ case
  _ : ts -> Just ((), ts)
  ts -> Just ((), ts)

empty :: Parser a
empty = Parser $ const Nothing

readValue :: Read a => String -> Parser a
readValue = maybe empty pure . readMaybe

unexpected :: Token -> Parser a
unexpected _ = empty

-- unexpected :: HasCallStack => (Token, String) -> Parser a
-- unexpected = error . show
