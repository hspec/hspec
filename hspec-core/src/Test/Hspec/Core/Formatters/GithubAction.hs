{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Core.Formatters.GithubAction (
  withGithubActionFormatter
) where

import Test.Hspec.Core.Format
import Test.Hspec.Core.Formatters.Internal
import Test.Hspec.Core.Util (joinPath)

data ErrorCommand = ErrorCommand
  { title   :: !(Maybe String)
  , file    :: !(Maybe String)
  , line    :: !(Maybe Int)
  , col     :: !(Maybe Int)
  , message :: !String
  }

-- | Make a suitable error annotation from an hspec failure.
--
-- Not clear what to do with the Maybe Location here: do we use the one from
-- the Item, or this one? What if both or neither are available?
-- Also not clear whether to use itemInfo.
errorCommandFor :: Path -> Item -> Maybe Location -> FailureReason -> ErrorCommand
errorCommandFor specPath@(_nesting, requirement) item _mLoc reason = ErrorCommand
    { -- requirement is used because it should always be non-empty and meaningful
      title = Just requirement
      -- TBD when should we use mLoc?
    , file = locationFile   <$> itemLocation item
    , line = locationLine   <$> itemLocation item
    , col  = locationColumn <$> itemLocation item
    , message = unlines (messageHeaderLines ++ messageBodyLines)
    }
  where
    -- Use the path to give a message header, so that the message is never empty.
    -- Empty messages seem to cause github actions to ignore the annotation.
    messageHeaderLines :: [String]
    messageHeaderLines = joinPath specPath : if itemInfo item == "" then [] else [itemInfo item]
    messageBodyLines :: [String]
    messageBodyLines = case reason of
        NoReason -> []
        Reason str -> [str]
        ExpectedButGot preface expected actual ->
          let bodyLines =
                [ mconcat ["expected: ", expected]
                , mconcat ["     got: ", actual  ]
                ]
              headerLines = maybe [] (\x -> [x]) preface
          in  headerLines ++ bodyLines
        Error preface err ->
          let bodyLines = [show err]
              headerLines = maybe [] (\x -> [x]) preface
          in  headerLines ++ bodyLines


-- | The github actions command format.
formatErrorCommand :: ErrorCommand -> String
formatErrorCommand ec = mconcat
  [ "::error "
  , "title=", maybe "" escapeProperty (title ec), ","
  , "file=", maybe "" escapeProperty (file ec), ","
  , "line=", maybe "" show (line ec), ","
  , "col=", maybe "" show (col ec)
  , "::"
  , escapeData (message ec)
  -- FIXME should check if on windows and use \r\n, as this is what the
  -- github library does.
  , "\n"
  ]

{-

function escapeData(s: any): string {
  return toCommandValue(s)
    .replace(/%/g, '%25')
    .replace(/\r/g, '%0D')
    .replace(/\n/g, '%0A')
}

function escapeProperty(s: any): string {
  return toCommandValue(s)
    .replace(/%/g, '%25')
    .replace(/\r/g, '%0D')
    .replace(/\n/g, '%0A')
    .replace(/:/g, '%3A')
    .replace(/,/g, '%2C')
}
-}

escapeData :: String -> String
escapeData = (>>= replace)
  where
    replace '%'  = "%25"
    replace '\r' = "%0D"
    replace '\n' = "%0A"
    replace c    = [c]

escapeProperty :: String -> String
escapeProperty = (>>= replace)
  where
    replace '%'  = "%25"
    replace '\r' = "%0D"
    replace '\n' = "%0A"
    replace ':'  = "%3A"
    replace ','  = "%2C"
    replace c    = [c]

withGithubActionFormatter :: Formatter -> Formatter
withGithubActionFormatter fmtr = fmtr
  { formatterItemDone = \path item -> do
      formatterItemDone fmtr path item
      emitGithubActionAnnotation path item
  }

emitGithubActionAnnotation :: Path -> Item -> FormatM ()
emitGithubActionAnnotation path item = case itemResult item of
  Success -> pure ()
  Pending _ _ -> pure ()
  Failure mLoc reason -> write . formatErrorCommand $ errorCommandFor path item mLoc reason
