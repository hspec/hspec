{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Formatters.Pretty.Parser.Parser where

import           Prelude ()
import           Test.Hspec.Core.Compat

newtype Parser token a = Parser { runParser :: [token] -> Maybe (a, [token]) }
  deriving Functor

instance Applicative (Parser token) where
  pure a = Parser $ \ input -> Just (a, input)
  (<*>) = ap

instance Monad (Parser token) where
  return = pure
  p1 >>= p2 = Parser $ runParser p1 >=> uncurry (runParser . p2)

instance Alternative (Parser token) where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \ input -> runParser p1 input <|> runParser p2 input

satisfy :: (token -> Bool) -> Parser token token
satisfy p = Parser $ \ input -> case input of
  t : ts | p t -> Just (t, ts)
  _ -> Nothing

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

readA :: (Alternative m, Read a) => String -> m a
readA = maybe empty pure . readMaybe
