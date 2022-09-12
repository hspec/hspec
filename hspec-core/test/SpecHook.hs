module SpecHook (hook) where

import           Prelude ()
import           Helper

import           System.Environment (getEnvironment)

ignoreHspecConfig :: IO a -> IO a
ignoreHspecConfig action = do
  env <- getEnvironment
  let filteredEnv = ("IGNORE_DOT_HSPEC", "yes") : filter p env
  withEnvironment filteredEnv action
  where
    p (name, _value) = name == "COMSPEC" || name == "PATH"

hook :: Spec -> Spec
hook = aroundAll_ ignoreHspecConfig
