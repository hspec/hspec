module Test.Hspec.Pending where

newtype Pending = Pending (Maybe String)

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
