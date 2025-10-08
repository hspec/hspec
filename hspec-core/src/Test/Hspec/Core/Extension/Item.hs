module Test.Hspec.Core.Extension.Item {-# WARNING in "x-experimental" "This API is experimental." #-} (
-- * Types
  Item(..)
, Location(..)
, Params(..)
, ActionWith
, Progress
, ProgressCallback
, Result(..)
, ResultStatus(..)
, FailureReason(..)

-- * Operations
, isFocused
, pending
, pendingWith
, setAnnotation
, getAnnotation
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Spec hiding (pending, pendingWith)
import           Test.Hspec.Core.Tree

isFocused :: Item a -> Bool
isFocused = itemIsFocused

pending :: Item a -> Item a
pending item = item { itemExample = \ _params _hook _progress -> result }
  where
    result :: IO Result
    result = return $ Result "" (Pending Nothing Nothing)

pendingWith :: String -> Item a -> Item a
pendingWith reason item = item { itemExample = \ _params _hook _progress -> result }
  where
    result :: IO Result
    result = return $ Result "" (Pending Nothing (Just reason))

setAnnotation :: Typeable value => value -> Item a -> Item a
setAnnotation = setItemAnnotation

getAnnotation :: Typeable value => Item a -> Maybe value
getAnnotation = getItemAnnotation
