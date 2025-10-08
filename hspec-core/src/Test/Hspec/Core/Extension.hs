{-# OPTIONS_GHC -Wno-x-experimental #-}
-- | Stability: unstable
module Test.Hspec.Core.Extension {-# WARNING in "x-experimental" "This API is experimental." #-} (
-- * Lifecycle of a test run
{- |
A test run goes through four distinct phases:

1. A tree of spec items and a default `Config` are constructed in `SpecM`.
2. Command-line options are parsed.  This transforms the default `Config` that was constructed in phase 1.
3. The tree of spec items is transformed in various ways, depending on specific `Config` values.
4. Each spec item is executed and its result is reported to the user.

An extension can directly influence phase 1, and indirectly influence phases 2-4.

When writing extensions the following imports are recommended:

@
import "Test.Hspec.Core.Extension"
import "Test.Hspec.Core.Extension.Config" qualified as Config
import "Test.Hspec.Core.Extension.Option" qualified as Option
import "Test.Hspec.Core.Extension.Item" qualified as Item
import "Test.Hspec.Core.Extension.Spec" qualified as Spec
import "Test.Hspec.Core.Extension.Tree" qualified as Tree
@
-}

-- ** Phase 1: Constructing a spec tree
{- |
- An extension can use @Spec.`Test.Hspec.Core.Extension.Spec.mapItems`@ to transform spec items in phase 1.

    * @Item.`setAnnotation`@ can be used to add custom metadata to a spec `Item`.

- An extension can use `modifyConfig` to modify the default config.

    * @Config.`Test.Hspec.Core.Extension.Config.setAnnotation`@ can be used to add custom metadata to the default `Config`.
-}
  runIO
, modifyConfig

-- ** Phase 2: Parsing command-line options
{- |
An extension can use `registerOptions` during phase 1 to register custom command-line options, and as a consequence indirectly influence this phase.

* Options can use @Config.`Test.Hspec.Core.Extension.Config.setAnnotation`@ to add custom metadata to the `Config`.
-}
, Option
, registerOptions

-- ** Phase 3: Transforming the spec tree
{- |
An extension can use `registerTransformation` during phase 1 to indirectly influence this phase.
-}
, registerTransformation

-- ** Phase 4: Reporting results
{- |
An extension can register a [custom formatter](https://hspec.github.io/extending-hspec-formatter.html) in phase 1 to indirectly influence this phase.
-}

-- * Types
{- |
Operations on these types are provided by the following modules respectively:

* "Test.Hspec.Core.Extension.Config"
* "Test.Hspec.Core.Extension.Item"
* "Test.Hspec.Core.Extension.Spec"
* "Test.Hspec.Core.Extension.Tree"
-}
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
import           Test.Hspec.Core.Spec (SpecM, SpecWith, runIO, modifyConfig)
import qualified Test.Hspec.Core.Spec as Core
import qualified Test.Hspec.Core.Config.Definition as Core

import           Test.Hspec.Core.Extension.Config (Config)
import qualified Test.Hspec.Core.Extension.Config.Type as Config
import           Test.Hspec.Core.Extension.Option
import           Test.Hspec.Core.Extension.Item
import           Test.Hspec.Core.Extension.Tree

registerOptions :: HasCallStack => [Option] -> SpecWith a
registerOptions = Core.modifyConfig . Core.addExtensionOptions section . map liftOption
  where
    section = "OPTIONS FOR " <> package
    package = maybe "main" (CallStack.srcLocPackage . snd) CallStack.callSite

    liftOption :: Option -> Declarative.Option Core.Config
    liftOption = Config.unOption

{- |
Register a transformation that transforms the spec tree in phase 3.

The registered transformation can use:

- @Tree.`mapItems`@ to modify spec items
- @Tree.`filterItems`@ to discard spec items
- @Config.`Test.Hspec.Core.Extension.Config.getAnnotation`@ to access custom config metadata
- @Item.`getAnnotation`@ to access custom item metadata
-}
registerTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> SpecWith a
registerTransformation = modifyConfig . Config.addSpecTransformation
