module Test.Hspec.Core.Extension (
-- * Register command-line options
  Option
, OptionSetter
, registerOption
, flag
, option
, argument

-- * Register tree transformations
, runIO
, modifyConfig
, registerTransformation

-- * Types
-- |
-- Operations on these types are provided by the following modules
-- respectively:
--
-- * "Test.Hspec.Core.Extension.Config"
-- * "Test.Hspec.Core.Extension.Item"
-- * "Test.Hspec.Core.Extension.Spec"
-- * "Test.Hspec.Core.Extension.Tree"
, Config
, Item
, SpecM
, SpecWith
, SpecTree
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified Data.CallStack as CallStack

import qualified GetOpt.Declarative as Declarative
import           Test.Hspec.Core.Spec (SpecM, SpecWith, runIO)
import qualified Test.Hspec.Core.Spec as Core
import qualified Test.Hspec.Core.Config.Definition as Core
import           Test.Hspec.Core.Extension.Config (Config)
import qualified Test.Hspec.Core.Extension.Config as Config
import qualified Test.Hspec.Core.Extension.Config.Type as Config
import           Test.Hspec.Core.Extension.Item (Item)
import           Test.Hspec.Core.Extension.Tree (SpecTree)

newtype Option = Option { unOption :: Declarative.Option Config }
newtype OptionSetter = OptionSetter { unOptionSetter :: Declarative.OptionSetter Config }

flag :: String -> (Bool -> Config -> Config) -> String -> Option
flag name setter = Option . Core.flag name setter

option :: String -> OptionSetter -> String -> Option
option name setter = Option . Core.option name (unOptionSetter setter)

argument :: String -> (String -> Maybe a) -> (a -> Config -> Config) -> OptionSetter
argument name parser setter = OptionSetter (Core.argument name parser setter)

registerOption :: HasCallStack => Option -> SpecWith a
registerOption = Core.modifyConfig . Core.addExtensionOptions section . return . liftOption
  where
    section = "OPTIONS FOR " <> package
    package = maybe "main" (CallStack.srcLocPackage . snd) CallStack.callSite

    liftOption :: Option -> Declarative.Option Core.Config
    liftOption = Declarative.mapOption Config.from Config.to . unOption

modifyConfig :: (Config -> Config) -> SpecWith a
modifyConfig f = Core.modifyConfig (Config.to . f . Config.from)

registerTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> SpecWith a
registerTransformation = modifyConfig . Config.addSpecTransformation
