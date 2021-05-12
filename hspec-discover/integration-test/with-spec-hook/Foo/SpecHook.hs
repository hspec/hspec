module Foo.SpecHook where

import Test.Hspec

hook :: SpecWith String -> SpecWith Int
hook = mapSubject show
