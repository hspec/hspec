{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.Diff
-- Copyright   :  (c) Sterling Clover 2008-2011, Kevin Charter 2011
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of the O(ND) diff algorithm as described in
-- \"An O(ND) Difference Algorithm and Its Variations (1986)\"
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927>. It is O(mn) in space.
-- The algorithm is the same one used by standared Unix diff.
-----------------------------------------------------------------------------

module Data.Algorithm.Diff
    ( Diff(..)
    -- * Comparing lists for differences
    , getDiff
    , getDiffBy

    -- * Finding chunks of differences
    , getGroupedDiff
    , getGroupedDiffBy
    ) where

import Prelude hiding (pi)

import Control.Applicative (liftA2)
import Data.Array

data DI = F | S | B deriving (Show, Eq)

-- | A value is either from the 'First' list, the 'Second' or from 'Both'.
-- 'Both' contains both the left and right values, in case you are using a form
-- of equality that doesn't check all data (for example, if you are using a
-- newtype to only perform equality on side of a tuple).
data Diff a = First a | Second a | Both a a deriving (Show, Eq, Functor)

data DL = DL {longest :: !Int, path :: !DI} deriving (Show, Eq)

lcs :: (a -> a -> Bool) -> [a] -> [a] -> [DI]
lcs eq as bs = back lena lenb
    where lena = length as; lenb = length bs

          arAs = listArray (0, lena-1) as
          arBs = listArray (0, lenb-1) bs

          dp = listArray ((0, 0), (lena, lenb)) $ liftA2 step [0..lena] [0..lenb]

          step 0 0 = DL 0 B
          step 0 _ = DL 0 S
          step _ 0 = DL 0 F
          step r c
            | (arAs!(lena-r)) `eq` (arBs!(lenb-c))
                        = let n = longest $ dp!(r-1, c-1)
                          in DL (n+1) B
            | otherwise = let n1 = longest $ dp!(r-1, c)
                              n2 = longest $ dp!(r, c-1)
                          in if n1 >= n2
                                then DL n1 F
                                else DL n2 S

          back 0 0 = []
          back r c = case path $ dp!(r, c) of
                        F -> F : back (r-1)   c
                        S -> S : back   r   (c-1)
                        B -> B : back (r-1) (c-1)

-- | Takes two lists and returns a list of differences between them. This is
-- 'getDiffBy' with '==' used as predicate.
getDiff :: (Eq t) => [t] -> [t] -> [Diff t]
getDiff = getDiffBy (==)

-- | Takes two lists and returns a list of differences between them, grouped
-- into chunks. This is 'getGroupedDiffBy' with '==' used as predicate.
getGroupedDiff :: (Eq t) => [t] -> [t] -> [Diff [t]]
getGroupedDiff = getGroupedDiffBy (==)

-- | A form of 'getDiff' with no 'Eq' constraint. Instead, an equality predicate
-- is taken as the first argument.
getDiffBy :: (t -> t -> Bool) -> [t] -> [t] -> [Diff t]
getDiffBy eq a b = markup a b $ lcs eq a b
    where markup (x:xs)   ys   (F:ds) = First x  : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = Second y : markup xs ys ds
          markup (x:xs) (y:ys) (B:ds) = Both x y : markup xs ys ds
          markup _ _ _ = []

getGroupedDiffBy :: (t -> t -> Bool) -> [t] -> [t] -> [Diff [t]]
getGroupedDiffBy eq a b = go $ getDiffBy eq a b
    where go (First x  : xs) = let (fs, rest) = goFirsts  xs in First  (x:fs)     : go rest
          go (Second x : xs) = let (fs, rest) = goSeconds xs in Second (x:fs)     : go rest
          go (Both x y : xs) = let (fs, rest) = goBoth    xs
                                   (fxs, fys) = unzip fs
                               in Both (x:fxs) (y:fys) : go rest
          go [] = []

          goFirsts  (First x  : xs) = let (fs, rest) = goFirsts  xs in (x:fs, rest)
          goFirsts  xs = ([],xs)

          goSeconds (Second x : xs) = let (fs, rest) = goSeconds xs in (x:fs, rest)
          goSeconds xs = ([],xs)

          goBoth    (Both x y : xs) = let (fs, rest) = goBoth xs    in ((x,y):fs, rest)
          goBoth    xs = ([],xs)
