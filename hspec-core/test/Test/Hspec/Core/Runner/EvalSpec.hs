{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.EvalSpec (spec) where

import           Prelude ()
import           Helper

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import qualified Data.Foldable as F (toList)
import           Data.List (permutations, sort)
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Format as H (Format(..), FormatConfig(..), SomeFormat(..))
import           Test.Hspec.Core.Formatters as H
import           Test.Hspec.Core.Runner.Eval
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Spec as H

spec :: Spec
spec = do
  let mkCfg asyncFmt f = do
        (traceF, getLines) <- traceFormatterIO asyncFmt
        let cfg = H.defaultConfig{H.configFormatter = Just traceF}
        return (f cfg, getLines)
      synchronousCfg n = mkCfg False $ \cfg -> cfg{H.configConcurrentJobs = Just n}
      parallelCfg   = mkCfg True $ \cfg -> cfg{H.configConcurrentJobs = Just 8}

  describe "traverse" $ do
    context "when used with Tree" $ do
      let
        tree :: Tree () Int
        tree = Node "" [Node "" [Leaf 1, Node "" [Leaf 2, Leaf 3]], Leaf 4]
      it "walks the tree left-to-right, depth-first" $ do
        ref <- newIORef []
        traverse_ (modifyIORef ref . (:) ) tree
        reverse <$> readIORef ref `shouldReturn` [1 .. 4]

  describe "synchronous reporting" $ do
    describe "runSequentially" $ do
      it "runs actions sequentially" $ do
        ref <- newIORef []
        (_, actionA) <- runSequentially mempty mempty $ \ _ -> threadDelay 10 >> modifyIORef ref (23 :)
        threadDelay 20
        (_, actionB) <- runSequentially mempty mempty $ \ _ -> threadDelay 10 >> modifyIORef ref (42 :)
        void $ waitFor actionB
        readIORef ref `shouldReturn` [42 :: Int]
        void $ waitFor actionA
        readIORef ref `shouldReturn` [23, 42]

    context "without -j" $
      modelProperty (synchronousCfg 1) "trace is sequential" (sequentialModel <$> resize 15 arbitrary) $
        \model -> (`shouldBe` modelSequentialTrace model)

  describe "asynchronous reporting" $
    context "with -j" $
      traceProperties parallelCfg
  where
    waitFor action = do
      progress_ <- asyncCellTake action
      case progress_ of
        Return{}  -> return ()
        Partial{} -> waitFor action


traceProperties :: IO (H.Config, IO [FormatterLine]) -> Spec
traceProperties mkCfg = do
  traceProperty mkCfg "trace is a reordering of sequential trace" $ \model tr ->
    sort tr == sort (modelSequentialTrace model)

  traceProperty mkCfg "group starts before children" $ \_model ->
    fst . flip foldl' (True, []) (\(allGood, groups) x -> case x of
      ExampleGroupStarted path name -> (allGood, (path ++ [name]) : groups)
      _ -> (allGood, groups))

modelProperty :: IO (H.Config, IO t) -> String -> Gen (SpecModel ()) -> (SpecModel () -> t -> IO b) -> Spec
modelProperty mkCfg name gen expectation = forAllUnique name gen $ \model -> ioProperty $ do
  modelSpec <- modelPermutations model
  forM_ (specInterleavings modelSpec) $ \p -> do
    (cfg, getLines) <- mkCfg
    H.hspecWith cfg (specDec modelSpec) `concurrently_` p
    outputTrace <- getLines
    expectation model outputTrace

traceProperty :: Show a => IO (H.Config, IO a) -> String -> (SpecModel () -> a -> Bool) -> Spec
traceProperty mkCfg name predicate =
  modelProperty mkCfg name (resize 15 arbitrary) (\model -> (`shouldSatisfy` predicate model))

------------------------------------------------------------------------------

data FormatterLine
  = ExampleGroupStarted [String] String
  | ExampleGroupDone
  | ExampleProgress H.Path (Int,Int)
  | ExampleSucceeded H.Path String
  | ExampleFailed  H.Path String String
  | ExamplePending H.Path String (Maybe String)
  deriving (Eq, Ord, Show)

traceFormatterIO :: Bool -> IO (FormatConfig -> IO SomeFormat, IO [FormatterLine])
traceFormatterIO asyncFmt = do
  ref <- newIORef []
  let push x = liftIO $ modifyIORef' ref (x :)
      f cfg = do
        SomeFormat fmt <- H.toFormatter cfg $ silent
          { exampleGroupStarted = (push.) . ExampleGroupStarted
          , exampleGroupDone    = push ExampleGroupDone
          , exampleProgress     = (push.) . ExampleProgress
          , exampleSucceeded    = (push.) . ExampleSucceeded
          , exampleFailed       = \path info reason -> push $ ExampleFailed path info (show reason)
          , examplePending      = ((push.).) . ExamplePending
          }
        return $ SomeFormat fmt{formatAsynchronously = asyncFmt}
  return (f, reverse <$> readIORef ref)

------------------------------------------------------------------------------
-- Hack to avoid checking a property with the same input more than once
forAllUnique :: (Show t, Eq t) => String -> Gen t -> (t -> Property) -> Spec
forAllUnique title gen prop =
  -- there has to be a better way of doing this
  beforeAll (newIORef []) $
    it title $ \seenRef -> do
      let alreadyChecked x = atomicModifyIORef seenRef $ \xx ->
            if x`elem` xx then (xx, True) else (x : xx, False)
      forAll gen $ \x -> ioProperty $ do
        already <- alreadyChecked x
        return $ if already then property Discard else prop x

---------------------------------------------------

data SpecModel  a = Describe {steps::[SpecModel' a], isParallel :: !Bool}
  deriving (Eq, Show)

instance Traversable SpecModel where
  traverse f (Describe steps p) = flip Describe p <$> (traverse.traverse) f steps
instance Foldable SpecModel where foldMap = foldMapDefault
instance Functor SpecModel where fmap = fmapDefault

data SpecModel' a = Success a | Nest (SpecModel a)
  deriving (Eq, Show)

instance Traversable SpecModel' where
  traverse f (Success a)  = Success <$> f a
  traverse f (Nest model) = Nest <$> traverse f model
instance Foldable SpecModel' where foldMap = foldMapDefault
instance Functor SpecModel' where fmap = fmapDefault

sequentialModel :: SpecModel a -> SpecModel a
sequentialModel (Describe steps _) = Describe (map go steps) False
  where
    go (Nest m) = Nest (sequentialModel m)
    go x = x

modelSequentialTrace :: SpecModel a -> [FormatterLine]
modelSequentialTrace = go [] (1::Int) where
  go _     _  steps | null $ F.toList steps = []
  go path idx (Describe {steps}) =
    ExampleGroupStarted path name :
    concat (zipWith (go' (path++[name])) [1..] steps) ++
    [ExampleGroupDone]
    where
      name = "Group " ++ show idx
  go' path idx (Success _) =
    [ ExampleSucceeded (path, "Example " ++ show idx) ""]
  go' path idx (Nest model) = go path idx model

genSpecModel :: Arbitrary a => Int -> Gen (SpecModel a)
genSpecModel size = do
  n <- choose (0, min 5 size)
  steps <- vectorOf n (genSpecModel' (size `div` n))
  Describe steps <$> arbitrary

genSpecModel' :: Arbitrary a => Int -> Gen (SpecModel' a)
genSpecModel' size = oneof $
  [ Success <$> arbitrary ] ++
  [ Nest <$> genSpecModel (size `div` 2) | size > 1]

instance (Arbitrary a, Monoid a) => Arbitrary (SpecModel a) where
  arbitrary =
    sized genSpecModel `suchThat` \(Describe {steps}) -> not $ null(F.toList steps)
  shrink (Describe steps isParallel) =
    [Describe steps' isParallel | steps' <- shrink steps] ++
    [Describe steps False | isParallel]

instance (Arbitrary a, Monoid a) => Arbitrary (SpecModel' a) where
  arbitrary = sized genSpecModel'
  shrink (Nest x) = Success mempty : (Nest <$> shrink x)
  shrink Success{} = []

toSpec :: Int -> SpecModel (IO ()) -> H.Spec
toSpec idx (Describe {steps, isParallel}) =
  (if isParallel then H.parallel else id) $
    H.describe ("Group " ++ show idx) . sequence_ $ zipWith toSpec' [1 ..] steps

toSpec' :: Int -> SpecModel' (IO ()) -> H.Spec
toSpec' idx (Success x) = H.it ("Example " ++ show idx) x
toSpec' idx (Nest s) = toSpec idx s

data SpecPermutations = SpecPermutations
  { specDec  :: H.Spec           -- ^ A spec
  , specInterleavings :: [IO ()] -- ^ All possible test completion orderings in 'specDec'
  }

modelPermutations :: SpecModel () -> IO SpecPermutations
modelPermutations model = do
  mvarsTree <- traverse (const newEmptyMVar) model
  let mvars = toList mvarsTree
      specDec = toSpec 1 $ takeMVar <$> mvarsTree
      specInterleavings = traverse_ (\v -> threadDelay 2 >> putMVar v ()) <$> permutations mvars
  return SpecPermutations {..}
