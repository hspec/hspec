-----------------------------------------------------------------------------
--
-- Module      :  Test.Hspec
-- Copyright   :  (c) Trystan Spangler 2011
-- License     :  modified
--
-- Maintainer  : trystan.s@comcast.net
-- Stability   : experimental
-- Portability : portable
--
-- |
--
-----------------------------------------------------------------------------

module Test.Hspec (
  Spec(), Result(), describe, it, pending, pureHspec, hHspec, hspec
) where

import Test.Hspec.Internal
