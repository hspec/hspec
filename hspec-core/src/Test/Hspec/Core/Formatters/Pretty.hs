{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
          Number _ -> False
          Operator _ _ _ -> True
          Record _ _ -> True
          Constructor _ xs -> any go xs
          Tuple xs -> any go xs
          List _ -> True

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
renderValue unicode = runBuilder . render 0
  where
    render :: Int -> Value -> Builder
    render indentation = \ case
      Char c -> shows c
      String str -> if unicode then Builder $ ushows str else shows str
      Number n -> fromString n
      Operator a name b -> render indentation a <> " " <> fromString name <> " " <> render indentation b
      Record name fields -> fromString name <> renderFields fields
      Constructor name values -> intercalate " " (fromString name : map (render indentation) values)
      Tuple [e@Record{}] -> render indentation e
      Tuple xs -> "(" <> intercalate ", " (map (render indentation) xs) <> ")"
      List [] -> "[]"
      List xs -> "[\n  " <> intercalate ",\n  " (map (render indentation) xs) <> ",\n]"
      where
        spaces :: Builder
        spaces = indentBy (succ indentation)

        renderFields :: [(String, Value)] -> Builder
        renderFields fields = mconcat [
            " {\n" <> spaces
          , intercalate (",\n" <> spaces) $ map renderField fields
          , "\n" <> indentBy indentation <> "}"
          ]

        renderField :: (String, Value) -> Builder
        renderField (name, value) = fromString name <> " = " <> render (succ indentation) value

    indentBy :: Int -> Builder
    indentBy n = fromString $ replicate (2 * n) ' '
