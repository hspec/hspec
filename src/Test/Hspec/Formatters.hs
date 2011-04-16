module Test.Hspec.Formatters where

-- | This module contains formaatters that take a set of specs and write to a given handle.
-- They follow a structure similar to RSpec formatters.
--
import Test.Hspec.Core
import System.IO
import Data.List (intersperse)
import Text.Printf
import Control.Monad (when)
import System.Console.ANSI

specdoc :: Bool -> Formatter
specdoc useColor = Formatter {
  exampleGroupStarted = \ h spec -> do
    when useColor (normalColor h)
    hPutStr h ('\n' : name spec ++ "\n"),

  examplePassed = \ h spec _ -> do
    when useColor (passColor h)
    hPutStrLn h $ " - " ++ requirement spec,

  exampleFailed = \ h spec errors -> do
    when useColor (failColor h)
    hPutStrLn h $ " x " ++ requirement spec ++ " FAILED [" ++ (show $ (length errors) + 1) ++ "]",

  examplePending = \ h spec _ -> do
    when useColor (pendingColor h)
    let (Pending s) = result spec
    hPutStrLn h $ " - " ++ requirement spec ++ "\n     # " ++ s,

  errorsFormatter = \ h errors -> do
    when useColor (failColor h)
    mapM_ (hPutStrLn h) ([""] ++ intersperse "" errors)
    when (not $ null errors) (hPutStrLn h ""),

  footerFormatter = \ h specs time -> do
    when useColor (if failedCount specs == 0 then passColor h else failColor h)
    hPutStrLn h $ printf "Finished in %1.4f seconds" time
    hPutStrLn h ""
    hPutStr   h $ quantify (length specs) "example" ++ ", "
    hPutStrLn h $ quantify (failedCount specs) "failure"
    when useColor (normalColor h)
  }


failColor :: Handle -> IO()
failColor h = hSetSGR h [ SetColor Foreground Dull Red ]

passColor :: Handle -> IO()
passColor h = hSetSGR h [ SetColor Foreground Dull Green ]

pendingColor :: Handle -> IO()
pendingColor h = hSetSGR h [ SetColor Foreground Dull Yellow ]

normalColor :: Handle -> IO()
normalColor h = hSetSGR h [ Reset ]
