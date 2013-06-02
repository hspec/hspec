module Test.Hspec.Runner.Eval where

import           Control.Monad
import           Control.Applicative
import qualified Control.Exception as E

import           Control.Monad.IO.Class (liftIO)

import           Test.Hspec.Util
import           Test.Hspec.Core.Type
import           Test.Hspec.Config
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.Timer

-- | Evaluate all examples of a given spec and produce a report.
runFormatter :: Bool -> Config -> Formatter -> [SpecTree] -> FormatM ()
runFormatter useColor c formatter specs = headerFormatter formatter >> zip [0..] specs `each` go []
  where
    -- like forM_, but respects --fast-fail
    each :: [a] -> (a -> FormatM ()) -> FormatM ()
    each []     _ = pure ()
    each (x:xs) f = do
      f x
      fails <- getFailCount
      unless (configFastFail c && fails /= 0) $ do
        xs `each` f

    eval :: IO Result -> FormatM (Either E.SomeException Result)
    eval
      | configDryRun c = \_ -> return (Right Success)
      | otherwise      = liftIO . safeTry . fmap forceResult

    go :: [String] -> (Int, SpecTree) -> FormatM ()
    go rGroups (n, SpecGroup group xs) = do
      exampleGroupStarted formatter n (reverse rGroups) group
      zip [0..] xs `each` go (group : rGroups)
      exampleGroupDone formatter
    go rGroups (_, SpecItem requirement example) = do
      progressHandler <- mkProgressHandler
      result <- eval (example $ Params (configQuickCheckArgs c) progressHandler)
      case result of
        Right Success -> do
          increaseSuccessCount
          exampleSucceeded formatter path
        Right (Pending reason) -> do
          increasePendingCount
          examplePending formatter path reason

        Right (Fail err) -> failed (Right err)
        Left e           -> failed (Left  e)
      where
        path = (groups, requirement)
        groups = reverse rGroups
        failed err = do
          increaseFailCount
          addFailMessage path err
          exampleFailed  formatter path err

        mkProgressHandler
          | useColor = do
              timer <- liftIO $ newTimer 0.05
              return $ \p -> do
                f <- timer
                when f $ do
                  exampleProgress formatter (configHandle c) path p
          | otherwise = return . const $ return ()
