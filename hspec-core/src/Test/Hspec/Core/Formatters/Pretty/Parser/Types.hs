module Test.Hspec.Core.Formatters.Pretty.Parser.Types where

import           Prelude ()
import           Test.Hspec.Core.Compat

data Value =
    Char Char
  | String String
  | Integer Integer
  | Rational String
  | Id String
  | App Value Value
  | Parentheses Value
  | Tuple [Value]
  | List [Value]
  | Record String [(String, Value)]
  deriving (Eq, Show)
