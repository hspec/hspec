{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Test.Hspec.Core.Formatters.TextBlock
  ( TextSpan(..)
  , TextBlock(..)
  , textBlock
  , TextBlocks
  , textBlocks
  , appendTextBlock
  , insertTextBlockAt
  , modifyTextBlock
  , textBlocksCount
  , Style(..)
  , line
  , lineS
  , renderTextSpan
  , cursorUp
  , TextF(..)
  , TextEnvironment(..)
  , interpretText
  , cursorDown
  , renderTextBlock
  , renderTextBlocks
  , reRenderTextBlocks
  , withStyle
  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor
  )where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.Algorithm.Diff             as Diff
import           Data.Bool
import           Data.Eq
import           Data.Foldable                   (Foldable(foldMap), traverse_)
import           Data.Function
import           Data.Int
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as M
import           Data.List                       ((++), concatMap, filter, last, map, mapAccumR, null, span, sum, zip)
import           Data.Maybe
import           Data.Monoid                     (Monoid(..), Sum(..))
import           Data.Ord
import           Data.Semigroup                  (Semigroup(..))
import           Data.String
import           Data.Tuple
import           Test.Hspec.Core.Formatters.Free
import           Text.Show

import Prelude (Enum(..), Num(..))

data Style = SuccessStyle
           | FailureStyle
           | PendingStyle
           | InfoStyle
           | PlainStyle
           | DiffMissingStyle
           | DiffExtraStyle
  deriving (Eq, Show)

data TextSpan = TextSpan { tsText :: !String, tsStyle :: !Style}
  deriving (Eq)

instance Show TextSpan where
  show TextSpan{..}
    | tsStyle == PlainStyle = tsText
    | otherwise = braces(show tsStyle) <> " " <> tsText
instance IsString TextSpan where fromString x = TextSpan x PlainStyle

-- | A block of text in the report that can be addressed and 'rewrite'n
--   Text blocks are always terminated with a newline.
data TextBlock a = TextBlock { textBlockValue :: a, getTextSpans :: ![TextSpan]}
  deriving (Eq, Functor)

instance Show (TextBlock a) where show = concatMap show . getTextSpans

textBlock :: [TextSpan] -> TextBlock ()
textBlock = TextBlock ()

instance a ~ () => IsString (TextBlock a) where fromString = TextBlock () . (:[]) . fromString

instance Semigroup (TextBlock a) where
  tb <> tb' = TextBlock (textBlockValue tb') (getTextSpans tb <> getTextSpans tb')

instance Monoid a => Monoid (TextBlock a) where
  mempty = TextBlock mempty []
  mappend = (<>)

instance Applicative TextBlock where
  pure = flip TextBlock []
  TextBlock f ts <*> TextBlock a ts' = TextBlock (f a) (ts <> ts')

instance Monad TextBlock where
  -- | empty textblock with no spans
  return = pure
  -- | span concatenation
  tb >>= k =
    let tb' = k(textBlockValue tb) in tb'{getTextSpans = getTextSpans tb <> getTextSpans tb'}

line :: TextBlock () -> TextBlock ()
line s = s <> "\n"

lineS :: String -> TextBlock ()
lineS s = fromString s <> "\n"

countNewlines :: String -> Int
countNewlines = sum . map (\x -> if x == '\n' then 1 else 0)

isEndedInNewline :: TextBlock a -> Bool
isEndedInNewline (TextBlock _ []) = False
isEndedInNewline (TextBlock _ spans) =
  case tsText(last spans) of
    [] -> False
    s  -> last s == '\n'

tbHeight :: TextBlock a -> Int
tbHeight tb = fromSpans + fromBlock
  where
    fromSpans = getSum . foldMap (Sum . countNewlines . tsText) . getTextSpans $ tb
    fromBlock = if isEndedInNewline tb then 0 else 1

-- | Returns the list of textblocks, each annotated by the distance in lines to the bottom of the screen
annotatedByDistance :: TextBlocks -> [(TextBlock (), Int)]
annotatedByDistance tbb = snd $ mapAccumR (\s tb -> let s' = s + tbHeight tb in (s', (tb, s'))) 0 (getTextBlocks tbb)

data TextBlocks = TextBlocks {theTextBlocks :: IntMap (TextBlock ()), theOrder :: [Int]}

textBlocks :: [TextBlock ()] -> TextBlocks
textBlocks bb = TextBlocks (M.fromList bb') (fst <$> bb')
  where bb' = zip [0..] bb

getTextBlocks :: TextBlocks -> [TextBlock ()]
getTextBlocks tb = map (\i -> M.findWithDefault mempty i (theTextBlocks tb)) (theOrder tb)

instance Eq TextBlocks where
  a == b = map normalize (getTextBlocks a) == map normalize (getTextBlocks b)
    where
      onLast f [] = f Nothing
      onLast f [x] = f (Just x)
      onLast f (x:xx) = x : onLast f xx

      normalize (TextBlock () tss) = TextBlock () $ onLast (maybe ["\n"] ((:[]) . normalizeLastSpan)) tss
      normalizeLastSpan ts = ts{tsText = onLast (maybe "\n" endInNewline) (tsText ts)}
      endInNewline '\n' = "\n"
      endInNewline x    = x : "\n"

instance Show TextBlocks where show = show . getTextBlocks

instance Monoid TextBlocks where
  mempty = TextBlocks mempty mempty
  mappend = (<>)

instance Semigroup TextBlocks where
  tb <> tb' = TextBlocks theTextBlocks' theOrder' where
    l        = getNextKey tb
    theOrder' = theOrder tb ++ map (+ l) (theOrder tb')
    theTextBlocks' = theTextBlocks tb <> M.fromList (map (first (+ l)) (M.toList $ theTextBlocks tb'))

textBlocksCount :: TextBlocks -> Int
textBlocksCount = M.size . theTextBlocks

appendTextBlock :: TextBlock () -> TextBlocks -> (TextBlocks, Int)
appendTextBlock t bb = (bb', last (theOrder bb'))
  where
    bb' = bb <> textBlocks [t]

getNextKey :: TextBlocks -> Int
getNextKey = succ . lookupMax . theTextBlocks
  where
    lookupMax x = if M.null x then 0 else fst $ M.findMax x

insertTextBlockAt :: Int -> TextBlock () -> TextBlocks -> (TextBlocks, Int)
insertTextBlockAt i tb bb = (bb', l)
  where
    bb' = TextBlocks (M.insert l tb (theTextBlocks bb)) (b ++ tb_i : l : a)
    l = getNextKey bb
    (b, tb_i: a) = span (/= i) (theOrder bb)

modifyTextBlock :: Int
                -> (TextBlock () -> Maybe (TextBlock ()))
                -> TextBlocks
                -> TextBlocks
modifyTextBlock n f bb = TextBlocks theTextBlocks' theOrder'
  where
    theTextBlocks' = M.update f n (theTextBlocks bb)
    theOrder'      = (if M.member n theTextBlocks' then id else filter (/= n)) (theOrder bb)

-- | Create a styled text block
withStyle :: Style -> String -> TextBlock ()
withStyle st s = TextBlock () [TextSpan s st]

-- | Create a cyan text block
withInfoColor :: String -> TextBlock ()
withInfoColor = withStyle InfoStyle
-- | Create a green text block
withSuccessColor :: String -> TextBlock ()
withSuccessColor = withStyle SuccessStyle
-- | Create a red text block
withFailColor :: String -> TextBlock ()
withFailColor = withStyle FailureStyle
-- | Create a yellow text block
withPendingColor :: String -> TextBlock ()
withPendingColor = withStyle PendingStyle

data TextF next
  = RenderTextSpan TextSpan next
  | CursorUp Int next
  | ClearLine next
  | ClearFromCursorToEnd next

instance Show (TextF ()) where
  show (RenderTextSpan (TextSpan t PlainStyle) ()) = show t
  show (RenderTextSpan TextSpan{..} ()) = show tsStyle <> braces (show tsText)
  show (ClearFromCursorToEnd ()) = "CLEAR DOWN"
  show (ClearLine ()) = "CLEAR LINE"
  show (CursorUp n _)
    | n > 0 = "↑" ++ show n
    | otherwise = "↓" ++ show (-n)

braces :: String -> String
braces s = '{' : s ++ "}"

instance Functor TextF where
  fmap f (RenderTextSpan ts next)    = RenderTextSpan ts $ f next
  fmap f (CursorUp n next)           = CursorUp n $ f next
  fmap f (ClearFromCursorToEnd next) = ClearFromCursorToEnd $ f next
  fmap f (ClearLine next)            = ClearLine $ f next

renderTextSpan       :: Monad m => TextSpan -> FreeT TextF m ()
cursorUp             :: Monad m => Int -> FreeT TextF m ()
clearFromCursorToEnd :: Monad m => FreeT TextF m ()
clearLine            :: Monad m => FreeT TextF m ()

renderTextSpan     s = liftF (RenderTextSpan s ())
cursorUp           n = liftF (CursorUp n ())
clearFromCursorToEnd = liftF (ClearFromCursorToEnd ())
clearLine            = liftF (ClearLine ())

cursorDown :: Monad m => Int -> FreeT TextF m ()
cursorDown n = cursorUp (-n)

renderTextBlock :: Monad m => TextBlock a -> FreeT TextF m ()
renderTextBlock tb = traverse_ renderTextSpan (getTextSpans tb) *> lastLine
  where
    lastLine = if isEndedInNewline tb then return () else renderTextSpan "\n"

renderTextBlocks :: Monad m => TextBlocks -> FreeT TextF m ()
renderTextBlocks = traverse_ renderTextBlock . getTextBlocks

reRenderTextBlocks :: Monad m => TextBlocks -> TextBlocks -> FreeT TextF m ()
reRenderTextBlocks old (getTextBlocks -> new) = worker 0 0 diffs
  where
    diffs = Diff.getDiffBy ((==) `on` fst) (annotatedByDistance old) (zip new [0..])
    worker _ cursor [] = cursorDown (max 0 cursor)
    worker _ cursor (Diff.Both (tb,pos) _ : rest) = worker (pos - tbHeight tb) cursor rest
    worker _ cursor (Diff.First (tb, pos) : Diff.Second (tb', _pos) : rest)
     -- modifying a line or extending the bottomest block is handled via in place mutation
      | tbHeight tb == tbHeight tb' || null rest = do
          cursorUp (pos - cursor)
          clearLine
          renderTextBlock tb'
          let pos' = pos - cursor - tbHeight tb'
          worker pos' pos' rest
      -- Adding / removing lines is handled by redrawing everything below the first line modified.
      | otherwise = do
          cursorUp (pos - cursor)
          clearFromCursorToEnd
          renderRest (Diff.Second (tb',_pos) : rest)

    -- adding to the end is handled via in place mutation
    worker _ cursor [Diff.Second (tb, _)] = do
      cursorDown cursor
      clearLine
      void $ renderTextBlock tb

    -- removing blocks causes the rest of the buffer to redraw
    worker _ cursor (Diff.First (_,pos) : rest) = do
      cursorUp (pos - cursor)
      clearFromCursorToEnd
      renderRest rest

    -- inserting causes the rest of the buffer to redraw
    worker pos cursor (b@Diff.Second{} : rest) = do
      cursorUp (pos - cursor)
      clearFromCursorToEnd
      renderRest (b : rest)

    renderRest rest = renderTextBlocks (textBlocks $ mapMaybe (fmap fst . scnd) rest)

    scnd (Diff.Both _ b) = Just b
    scnd  Diff.First{}   = Nothing
    scnd (Diff.Second x) = Just x

data TextEnvironment m = TextEnvironment {
  envRenderTextSpan       :: TextSpan -> m (),
  envMoveCursorUp         :: Int -> m (),
  envClearFromCursorToEnd :: m (),
  envClearLine            :: m ()
  }

interpretText :: Monad m => TextEnvironment m -> FreeT TextF m b -> m b
interpretText env = iterT (textAlg env)

textAlg :: Monad f => TextEnvironment f -> TextF (f b) -> f b
textAlg TextEnvironment{..} (RenderTextSpan ts k)    = envRenderTextSpan ts >> k
textAlg TextEnvironment{..} (CursorUp n k)           = envMoveCursorUp n >> k
textAlg TextEnvironment{..} (ClearFromCursorToEnd k) = envClearFromCursorToEnd >> k
textAlg TextEnvironment{..} (ClearLine            k) = envClearLine >> k
