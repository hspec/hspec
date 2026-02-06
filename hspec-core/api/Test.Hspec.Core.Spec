context :: HasCallStack => String -> SpecWith a -> SpecWith a
describe :: HasCallStack => String -> SpecWith a -> SpecWith a
fcontext :: HasCallStack => String -> SpecWith a -> SpecWith a
fdescribe :: HasCallStack => String -> SpecWith a -> SpecWith a
fit :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
focus :: SpecWith a -> SpecWith a
focusForest :: [SpecTree a] -> [SpecTree a]
fspecify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
getSpecDescriptionPath :: SpecM a [String]
it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
parallel :: SpecWith a -> SpecWith a
pending :: HasCallStack => Expectation
pendingWith :: HasCallStack => String -> Expectation
sequential :: SpecWith a -> SpecWith a
specify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xcontext :: HasCallStack => String -> SpecWith a -> SpecWith a
xdescribe :: HasCallStack => String -> SpecWith a -> SpecWith a
xit :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xspecify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
type ActionWith :: * -> *
type ActionWith a = a -> IO ()
type Example :: * -> Constraint
class Example e where
  type Arg :: * -> *
  type family Arg e
    Default: ()
  evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result
  {-# MINIMAL evaluateExample #-}
type Expectation :: *
type Expectation = Test.HUnit.Lang.Assertion
type FailureReason :: *
data FailureReason
  | NoReason
  | Reason String
  | ColorizedReason String
  | ExpectedButGot (Maybe String) String String
  | Error (Maybe String) GHC.Internal.Exception.Type.SomeException
type HasCallStack :: Constraint
type HasCallStack = ?callStack::GHC.Internal.Stack.Types.CallStack
type Item :: * -> *
data Item a = Item
  itemRequirement :: String
  itemLocation :: Maybe Location
  itemIsParallelizable :: Maybe Bool
  itemIsFocused :: Bool
  itemAnnotations :: hspec-core:Test.Hspec.Core.Annotations.Annotations
  itemExample :: Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
type Location :: *
data Location = Location
  locationFile :: FilePath
  locationLine :: Int
  locationColumn :: Int
type Params :: *
data Params = Params
  paramsQuickCheckArgs :: Test.QuickCheck.Test.Args
  paramsSmallCheckDepth :: Maybe Int
type Progress :: *
type Progress = (Int, Int)
type ProgressCallback :: *
type ProgressCallback = Progress -> IO ()
type Result :: *
data Result = Result
  resultInfo :: String
  resultStatus :: ResultStatus
type ResultStatus :: *
data ResultStatus
  | Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
type Spec :: *
type Spec = SpecWith ()
type role SpecM nominal nominal
type SpecM :: * -> * -> *
newtype SpecM a r = SpecM
  hspec-core:Test.Hspec.Core.Spec.Monad.unSpecM :: Control.Monad.Trans.Writer.Lazy.WriterT (GHC.Internal.Data.Semigroup.Internal.Endo hspec-core:Test.Hspec.Core.Config.Definition.Config, [SpecTree a]) (Control.Monad.Trans.Reader.ReaderT hspec-core:Test.Hspec.Core.Spec.Monad.Env IO) r
type SpecTree :: * -> *
type SpecTree a = Tree (IO ()) (Item a)
type SpecWith :: * -> *
type SpecWith a = SpecM a ()
type Tree :: * -> * -> *
data Tree c a
  | Node String [Tree c a]
  | NodeWithCleanup (Maybe (String, Location)) c [Tree c a]
  | Leaf a
bimapForest :: (a -> b) -> (c -> d) -> [Tree a c] -> [Tree b d]
bimapTree :: (a -> b) -> (c -> d) -> Tree a c -> Tree b d
defaultParams :: Params
filterForest :: (a -> Bool) -> [Tree c a] -> [Tree c a]
filterForestWithLabels :: ([String] -> a -> Bool) -> [Tree c a] -> [Tree c a]
filterTree :: (a -> Bool) -> Tree c a -> Maybe (Tree c a)
filterTreeWithLabels :: ([String] -> a -> Bool) -> Tree c a -> Maybe (Tree c a)
fromSpecList :: [SpecTree a] -> SpecWith a
location :: HasCallStack => Maybe Location
mapSpecForest :: ([SpecTree a] -> [SpecTree b]) -> SpecM a r -> SpecM b r
mapSpecItem :: (ActionWith a -> ActionWith b) -> (Item a -> Item b) -> SpecWith a -> SpecWith b
mapSpecItem_ :: (Item a -> Item b) -> SpecWith a -> SpecWith b
modifyConfig :: (hspec-core:Test.Hspec.Core.Config.Definition.Config -> hspec-core:Test.Hspec.Core.Config.Definition.Config) -> SpecWith a
modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
pruneForest :: [Tree c a] -> [Tree c a]
pruneTree :: Tree c a -> Maybe (Tree c a)
runIO :: IO r -> SpecM a r
runSpecM :: SpecWith a -> IO (GHC.Internal.Data.Semigroup.Internal.Endo hspec-core:Test.Hspec.Core.Config.Definition.Config, [SpecTree a])
safeEvaluate :: IO Result -> IO Result
safeEvaluateExample :: Example e => e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result
specGroup :: HasCallStack => String -> [SpecTree a] -> SpecTree a
specItem :: (HasCallStack, Example e) => String -> e -> SpecTree (Arg e)
