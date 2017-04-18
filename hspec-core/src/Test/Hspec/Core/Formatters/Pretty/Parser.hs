{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Formatters.Pretty.Parser (
  parser
, Aexp(..)
, Fbind
, Exp
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (exp)

import           Control.Monad

import           Language.Haskell.Lexer

parser :: String -> Maybe Aexp
parser = parse aexp

lexer :: String -> [Symbol]
lexer = rmPos . rmSpace . lexerPass0
  where
    rmPos = map (\ (token, (_pos, s)) -> (token, s))

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . runParser p . lexer

newtype P token a = Parser {runParser :: [token] -> Maybe (a, [token])}
  deriving Functor

type Symbol = (Token, String)

type Parser = P Symbol

instance Applicative (P token) where
  pure  = return
  (<*>) = ap

instance Monad (P token) where
  fail _ = Parser (\_ -> Nothing)
  return a  = Parser $ \input -> Just (a, input)
  p1 >>= p2 = Parser $ \input -> runParser p1 input >>= uncurry (runParser . p2)

instance Alternative (P token) where
  empty = Parser (\_ -> Nothing)
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

inspect :: (token -> Maybe a) -> P token a
inspect f = Parser $ \input -> case input of
  x : xs | Just a <- f x -> Just (a, xs)
  _ -> Nothing

extract :: Token -> Parser String
extract token = inspect $ \symbol -> case symbol of
  (t, a) | t  == token -> Just a
  _ -> Nothing

intlit :: Parser String
intlit = extract IntLit

reservedop :: String -> Parser ()
reservedop op = extract Reservedop >>= guard . (== op)

special :: String -> Parser ()
special sym = extract Special >>= guard . (== sym)

--

string :: Parser String
string = extract StringLit

type Exp = String
exp :: Parser Exp
exp = string <|> intlit

type Qcon = String
qcon :: Parser Qcon
qcon = extract Conid

type Qvar = String
qvar :: Parser String
qvar = extract Varid

-- 3.15.2 Construction Using Field Labels

data Aexp =
    List [Exp]
  | Record Qcon [Fbind]
  deriving (Eq, Show)

aexp :: Parser Aexp
aexp = list <|> record
  where
    list = List <$> (special "[" *> sepBy exp (special ",") <* special "]")

    record = Record <$> qcon <*> fbinds
    fbinds :: Parser [Fbind]
    fbinds = special "{" *> sepBy fbind (special ",") <* special "}"

type Fbind = (Qvar, Exp)
fbind :: Parser Fbind
fbind = (,) <$> qvar <* reservedop "=" <*> exp
