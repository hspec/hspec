module Test.Hspec.Core.Formatters.Pretty (pretty, pretty2) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (exp)

import           Data.List

import           Test.Hspec.Core.Formatters.Pretty.Parser

pretty2 :: String -> String -> (String, String)
pretty2 a b = case (parser a, parser b) of
  (Just ae, Just be) -> (prettyAexp ae, prettyAexp be)
  _ -> (a, b)

pretty :: String -> String
pretty input =  case parser input of
  Nothing -> input
  Just aexp -> prettyAexp aexp

prettyExp :: Exp -> String
prettyExp = id

prettyAexp :: Aexp -> String
prettyAexp (List xs) = "[\n" ++ pp xs ++ "\n]"
  where
    pp = intercalate ",\n" . map ("  " ++) . map prettyExp

prettyAexp (Record qcon fbinds) = qcon ++ " {\n" ++ prettyFbinds fbinds ++ "\n}"
  where
    prettyFbinds :: [Fbind] -> String
    prettyFbinds = intercalate ",\n" . map ("  " ++) . map prettyFbind

prettyFbind :: Fbind -> String
prettyFbind (qvar, exp) = qvar ++ " = " ++ prettyExp exp
