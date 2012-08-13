{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Pending where

import qualified Test.Hspec.Internal as Internal
import           Test.Hspec.Internal (Example(..))

-- NOTE: This is defined in a separate packages, because it clashes with
-- Result.Pending.

-- | A pending example.
newtype Pending = Pending (Maybe String)

instance Example Pending where
  evaluateExample c (Pending reason) = evaluateExample c (Internal.Pending reason)

instance Example (String -> Pending) where
  evaluateExample c _ = evaluateExample c (Pending Nothing)

-- | A pending example.
--
-- If you want to report on a behavior but don't have an example yet, use this.
--
-- > describe "fancyFormatter" [
-- >   it "can format text in a way that everyone likes" $
-- >     pending
-- > ]
--
-- You can give an optional reason for why it's pending.
--
-- > describe "fancyFormatter" [
-- >   it "can format text in a way that everyone likes" $
-- >     pending "waiting for clarification from the designers"
-- > ]
pending :: String -> Pending
pending = Pending . Just
