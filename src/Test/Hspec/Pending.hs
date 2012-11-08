{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Pending where

import qualified Test.Hspec.Core.Type as Core
import           Test.Hspec.Core.Type (Example(..))

-- NOTE: This is defined in a separate packages, because it clashes with
-- Result.Pending.

-- | A pending example.
newtype Pending = Pending (Maybe String)

instance Example Pending where
  evaluateExample c (Pending reason) = evaluateExample c (Core.Pending reason)

instance Example (String -> Pending) where
  evaluateExample c _ = evaluateExample c (Pending Nothing)

-- | A pending example.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
--
-- You can give an optional reason for why it's pending:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending "waiting for clarification from the designers"
pending :: String -> Pending
pending = Pending . Just
