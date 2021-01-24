{-# LANGUAGE CPP #-}
-- |
-- Experimental combinators, that may become part of the main distribution, if
-- they turn out to be useful for a wider audience.
module Test.Hspec.Expectations.Contrib (
-- * Predicates
-- | (useful in combination with `shouldSatisfy`)
  isLeft
, isRight
) where


#if MIN_VERSION_base(4,7,0)
import Data.Either
#else

isLeft :: Either a b -> Bool
{-# DEPRECATED isLeft "use Data.Either.Compat.isLeft from package base-compat instead" #-}
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
{-# DEPRECATED isRight "use Data.Either.Compat.isRight from package base-compat instead" #-}
isRight (Left  _) = False
isRight (Right _) = True
#endif
