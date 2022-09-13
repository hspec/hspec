module SpecHook where

import           Prelude ()
import           Helper

hook :: Spec -> Spec
hook = aroundAll_ (withEnvironment [("IGNORE_DOT_HSPEC", "yes")])
