#!/usr/bin/runhaskell 
> module Main where
> import Distribution.Simple
> import Distribution.PackageDescription(PackageDescription)
> import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
> import System.Cmd(system)
> import Distribution.Simple.LocalBuildInfo
>
> main :: IO ()
> main = defaultMainWithHooks hooks
>   where hooks = simpleUserHooks { runTests = runspecs }
>
> runspecs :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
> runspecs _ _ _ lbi = system "runhaskell ./Specs.hs" >> return ()
