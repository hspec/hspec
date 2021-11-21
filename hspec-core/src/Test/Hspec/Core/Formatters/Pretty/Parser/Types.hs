module Test.Hspec.Core.Formatters.Pretty.Parser.Types where

import           Prelude ()
import           Test.Hspec.Core.Compat

data Expression =
    Literal Literal
  | Id String
  | App Expression Expression
  | Parentheses Expression
  | Tuple [Expression]
  | List [Expression]
  | Record String [(String, Expression)]
  deriving (Eq, Show)

data Literal =
    Char Char
  | String String
  | Integer Integer
  | Rational Rational
  deriving (Eq, Show)
