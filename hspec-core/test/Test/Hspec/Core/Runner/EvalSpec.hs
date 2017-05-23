module Test.Hspec.Core.Runner.EvalSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Runner.Eval

spec :: Spec
spec = do
  describe "traverse" $ do
    context "when used with Tree" $ do
      let
        tree :: Tree () Int
        tree = Node "" [Node "" [Leaf 1, Node "" [Leaf 2, Leaf 3]], Leaf 4]
      it "walks the tree left-to-right, depth-first" $ do
        ref <- newIORef []
        traverse_ (modifyIORef ref . (:) ) tree
        reverse <$> readIORef ref `shouldReturn` [1 .. 4]

  describe "runSequentially" $ do
    it "runs actions sequentially" $ do
      ref <- newIORef []
      (_, actionA) <- runSequentially $ \ _ -> modifyIORef ref (23 :)
      (_, actionB) <- runSequentially $ \ _ -> modifyIORef ref (42 :)
      (_, ()) <- actionB (\_ -> return ())
      readIORef ref `shouldReturn` [42 :: Int]
      (_, ()) <- actionA (\_ -> return ())
      readIORef ref `shouldReturn` [23, 42]
