{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Annotations (
  Annotations
, setValue
, getValue
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Typeable
import           Data.Dynamic
import           Data.Map (Map)
import qualified Data.Map as Map

newtype Annotations = Annotations (Map TypeRep Dynamic)
  deriving (
#if MIN_VERSION_base(4,11,0)
    Semigroup,
#endif
    Monoid)

setValue :: Typeable value => value -> Annotations -> Annotations
setValue value (Annotations values) = Annotations $ Map.insert (typeOf value) (toDyn value) values

getValue :: forall value. Typeable value => Annotations -> Maybe value
getValue (Annotations values) = Map.lookup (typeOf (undefined :: value)) values >>= fromDynamic
