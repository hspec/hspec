module SpecHook where

import           Test.Hspec
import qualified MyFormatter

hook :: Spec -> Spec
hook = MyFormatter.use
