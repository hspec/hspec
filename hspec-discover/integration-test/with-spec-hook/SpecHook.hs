module SpecHook where

import Test.Hspec

hook :: SpecWith Int -> Spec
hook = before (return 23)
