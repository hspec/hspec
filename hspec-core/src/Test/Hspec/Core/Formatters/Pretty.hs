{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Core.Formatters.Pretty (
  pretty2
#ifdef TEST
, pretty
, recoverString
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (shows, intercalate)

import           Control.Arrow
import           Data.Char
import           Data.String
import           Data.List (intersperse)
import qualified Text.Show as Show

import           Text.Show.Unicode (urecover)
import           Test.Hspec.Core.Formatters.Pretty.Parser

pretty2 :: Bool -> String -> String -> (String, String)
pretty2 unicode expected actual = case (recoverString unicode expected, recoverString unicode actual) of
  (Just expected_, Just actual_) -> (expected_, actual_)
  _ -> case (pretty expected, pretty actual) of
    (Just expected_, Just actual_) -> (expected_, actual_)
    _ -> (rec expected, rec actual)
  where
    rec = if unicode then urecover else id

recoverString :: Bool -> String -> Maybe String
recoverString unicode input = case readMaybe input of
  Just r | shouldParseBack r -> Just r
  _ -> Nothing
  where
    shouldParseBack = (&&) <$> all isSafe <*> isMultiLine
    isMultiLine = lines >>> length >>> (> 1)
    isSafe c = (unicode || isAscii c) && (not $ isControl c) || c == '\n'

pretty :: String -> Maybe String
pretty = parseExpression >=> render_
  where
    render_ :: Expression -> Maybe String
    render_ expr = guard (shouldParseBack expr) >> Just (renderExpression expr)

    shouldParseBack :: Expression -> Bool
    shouldParseBack = go
      where
        go expr = case expr of
          Literal _ -> False
          Id _ -> False
          App (Id _) e -> go e
          App _ _ -> False
          Parentheses e -> go e
          Tuple xs -> any go xs
          List xs -> any go xs
          Record _ _ -> True

newtype Builder = Builder ShowS

instance Monoid Builder where
  mempty = Builder id
#if MIN_VERSION_base(4,11,0)
instance Semigroup Builder where
#endif
  Builder xs
#if MIN_VERSION_base(4,11,0)
    <>
#else
    `mappend`
#endif
    Builder ys = Builder (xs . ys)

runBuilder :: Builder -> String
runBuilder (Builder xs) = xs ""

intercalate :: Builder -> [Builder] -> Builder
intercalate x xs = mconcat $ intersperse x xs

shows :: Show a => a -> Builder
shows = Builder . Show.shows

instance IsString Builder where
  fromString = Builder . showString

renderExpression :: Expression -> String
renderExpression = runBuilder . render
  where
    renderLiteral lit = case lit of
      Char c -> shows c
      String str -> shows str
      Integer n -> shows n
      Rational n -> shows n

    render :: Expression -> Builder
    render expr = case expr of
      Literal lit -> renderLiteral lit
      Id name -> fromString name
      App a b -> render a <> " " <> render b
      Parentheses e -> "(" <> render e <> ")"
      Tuple xs -> "(" <> intercalate ", " (map render xs) <> ")"
      List xs -> "[" <> intercalate ", " (map render xs) <> "]"
      Record name fields -> fromString name <> " {\n  " <> (intercalate ",\n  " $ map renderField fields) <> "\n}"

    renderField (name, value) = fromString name <> " = " <> render value
