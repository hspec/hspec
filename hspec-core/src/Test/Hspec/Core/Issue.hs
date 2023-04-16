{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Issue (
  Issue
, new
, report
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Char
import           Data.Bits 
import           Numeric (showIntAtBase)
import           System.IO
import qualified System.Info as System

import           Test.Hspec.Core.Util

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "unreleased"
#endif

new :: String -> SomeException -> Issue
new title err = issue
  where
    issue :: Issue
    issue = Issue {..}

    issueTitle :: String
    issueTitle = title

    issueLabels :: [String]
    issueLabels = ["bug"]

    issueBody :: String
    issueBody = intercalate "\n" [
        "```"
      , formatException err
      , "```"
      , ""
      , "```"
      , "version: " <> CURRENT_PACKAGE_VERSION
      , "os: " <> System.os
      , "```"
      ]

report :: Issue -> IO ()
report issue@Issue{..} = do
  hPutStr stderr $ unlines [
      ""
    , "WARNING:"
    , ""
    , "  " <> issueTitle
    , ""
    , "  Please report this issue at:"
    , ""
    , "    " <> issueUrl issue
    , ""
    ]

data Issue = Issue {
  issueTitle :: String
, issueLabels :: [String]
, issueBody :: String
}

issueUrl :: Issue -> String
issueUrl Issue{..} = "https://github.com/hspec/hspec/issues/new?title=" <> urlEncode issueTitle <> "&body=" <> urlEncode issueBody <> "&labels=" <> labels
  where
    labels = intercalate "," $ map urlEncode issueLabels

urlEncode :: String -> String
urlEncode = escapeURIString isSafe
  where
    isSafe c =
         isDigit c
      || isAsciiUpper c
      || isAsciiLower c
      || c == '_'
      || c == '-'

------------------------------------------------------------------------------
-- Copied from Network.URI
------------------------------------------------------------------------------

-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> String           -- ^ the string to process
    -> String           -- ^ the resulting URI string
escapeURIString p = concatMap (escapeURIChar p)

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeURIChar :: (Char->Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 toChrHex n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
