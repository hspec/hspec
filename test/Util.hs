module Util where

import qualified System.IO.Silently as S

capture_ :: IO a -> IO [String]
capture_ = fmap (lines . fst) . S.capture
