module Test.Hspec.Core.Spec.Env (
    Env (..)
  , pushAncestor
  ) where

-- | Environment of a test definition, in the Spec monad
data Env = Env 
  { -- | List of ancestors of this test. Parent is the first element,
    -- grand-parent is the next, and so on with root test appearing at the last
    -- position.
    envAncestorGroups :: [String]
  }
  deriving (Eq, Show, Ord)

pushAncestor :: String -> Env -> Env
pushAncestor label (Env ancs) = Env $ label : ancs
