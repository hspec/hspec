#!/usr/bin/runhaskell
> module Main where
> import Distribution.Simple
> import Distribution.Simple.Setup
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
> import System.Cmd(system)
> import Distribution.Simple.LocalBuildInfo
> import Test.Hspec
> import Specs (specs)
>
> main :: IO ()
> main = defaultMainWithHooks hooks
>   where hooks = simpleUserHooks { preSDist = \ _ _     -> runspecs >> return emptyHookedBuildInfo }
>
> runspecs :: IO ()
> runspecs = specs >>= hspecX
