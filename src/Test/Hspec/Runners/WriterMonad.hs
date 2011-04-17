
-- | This module contains the runners that take a set of specs, evaluate their examples, and
-- report to a given handle.
--
module Test.Hspec.Runners.WriterMonad (
 SpecM, hspec, hHspec, hHspecWithFormat, describe, it
) where

import System.IO
import Test.Hspec.Core hiding (describe,it)
import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runners.ListMonad as ListMonad

import Control.Monad.Trans.Writer (Writer, execWriter, tell)

-- fully monadic DSL
type ItSpec = IO (String, Result)

type SpecM = Writer [IO [IO Spec]] ()

hspec :: SpecM -> IO [Spec]
hspec = ListMonad.hspec . runSpecM

hHspec :: Handle -> SpecM -> IO [Spec]
hHspec h = ListMonad.hHspec h . runSpecM

hHspecWithFormat :: Formatter -> Handle -> SpecM -> IO [Spec]
hHspecWithFormat f h specs = ListMonad.hHspecWithFormat f h (runSpecM specs)

runSpecM :: SpecM -> IO [IO Spec]
runSpecM specs = descriptions $ execWriter specs

describe :: String -> Writer [ItSpec] () -> SpecM
describe label action = tell [Core.describe label (execWriter action)]

it :: SpecVerifier v => String -> v -> Writer [ItSpec] ()
it label action = tell [Core.it label action]
