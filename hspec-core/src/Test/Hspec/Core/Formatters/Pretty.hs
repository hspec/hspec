{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Core.Formatters.Pretty (
  pretty2
#ifdef TEST
, pretty
, recoverString
, recoverMultiLineString
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (shows, intercalate)

import           Data.Char
import           Data.String
import           Data.List (intersperse)
import qualified Text.Show as Show

import           Test.Hspec.Core.Formatters.Pretty.Unicode
import           Test.Hspec.Core.Formatters.Pretty.Parser

pretty2 :: Bool -> String -> String -> (String, String)
pretty2 unicode expected actual = case (recoverMultiLineString unicode expected, recoverMultiLineString unicode actual) of
  (Just expected_, Just actual_) -> (expected_, actual_)
  _ -> case (pretty unicode expected, pretty unicode actual) of
    (Just expected_, Just actual_) | expected_ /= actual_ -> (expected_, actual_)
    _ -> (expected, actual)

recoverString :: String -> Maybe String
recoverString xs = case xs of
  '"' : _ -> case reverse xs of
    '"' : _ -> readMaybe xs
    _ -> Nothing
  _ -> Nothing

recoverMultiLineString :: Bool -> String -> Maybe String
recoverMultiLineString unicode input = case recoverString input of
  Just r | shouldParseBack r -> Just r
  _ -> Nothing
  where
    shouldParseBack = (&&) <$> all isSafe <*> isMultiLine
    isMultiLine = lines >>> length >>> (> 1)
    isSafe c = (unicode || isAscii c) && not (isControl c) || c == '\n'

pretty :: Bool -> String -> Maybe String
pretty unicode = parseValue >=> render_
  where
    render_ :: Value -> Maybe String
    render_ value = guard (shouldParseBack value) >> Just (renderValue unicode value)

    shouldParseBack :: Value -> Bool
    shouldParseBack = go
      where
        go value = case value of
          Char _ -> False
          String _ -> True
          Rational _ _ -> False
          Number _ -> False
          Record _ _ -> True
          Constructor _ xs -> any go xs
          Tuple xs -> any go xs
          List xs -> any go xs

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

renderValue :: Bool -> Value -> String
renderValue unicode = runBuilder . render
  where
    render :: Value -> Builder
    render value = case value of
      Char c -> shows c
      String str -> if unicode then Builder $ ushows str else shows str
      Rational n d -> render n <> " % " <> render d
      Number n -> fromString n
      Record name fields -> fromString name <> " {\n  " <> (intercalate ",\n  " $ map renderField fields) <> "\n}"
      Constructor name values -> intercalate " " (fromString name : map render values)
      Tuple [e@Record{}] -> render e
      Tuple xs -> "(" <> intercalate ", " (map render xs) <> ")"
      List xs -> "[" <> intercalate ", " (map render xs) <> "]"

    renderField (name, value) = fromString name <> " = " <> render value
