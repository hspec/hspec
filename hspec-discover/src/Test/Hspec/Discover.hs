{-# OPTIONS_HADDOCK hide #-}
module Test.Hspec.Discover (hspec, describe) where

import Test.Hspec.Monadic (Specs, hspecX, describe)

hspec :: Specs -> IO ()
hspec = hspecX
