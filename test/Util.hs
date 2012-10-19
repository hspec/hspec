module Util where

import qualified System.IO.Silently as S

capture_ :: IO a -> IO String
capture_ = fmap fst . S.capture

capture__ :: IO a -> IO [String]
capture__ = fmap lines . capture_
