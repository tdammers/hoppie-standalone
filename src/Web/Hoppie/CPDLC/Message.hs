{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Hoppie.CPDLC.Message
( CPDLCMessage (..)
, CPDLCPart (..)
, MessageTypeID
, parseCPDLCMessage
, renderCPDLCMessage
, raToBS
)
where

import Web.Hoppie.CPDLC.MessageTypes
import Web.Hoppie.Telex

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P (decimal)
import Data.Bifunctor
import Data.Void
import Data.Maybe

data CPDLCMessage =
  CPDLCMessage
    { cpdlcMIN :: Word
    , cpdlcMRN :: Maybe Word
    , cpdlcReplyOpts :: ReplyOpts
    , cpdlcParts :: [CPDLCPart]
    }
    deriving (Show, Read, Eq)

data CPDLCPart =
  CPDLCPart
    { cpdlcType :: MessageTypeID
    , cpdlcArgs :: [ByteString]
    }
    deriving (Show, Read, Eq)

parseCPDLCMessage :: ByteString -> Either String CPDLCMessage
parseCPDLCMessage src =
  first P.errorBundlePretty $ P.parse (cpdlcMessageP <* P.eof) "input" src

renderCPDLCMessage :: CPDLCMessage -> ByteString
renderCPDLCMessage cpdlc =
  mconcat $
    [ "/data2/"
    , (BS8.pack . show) (cpdlcMIN cpdlc)
    , "/"
    , maybe "" (BS8.pack . show) (cpdlcMRN cpdlc)
    , "/"
    , raToBS (cpdlcReplyOpts cpdlc)
    , "/"
    ] ++
    [ renderMessage allMessageTypes (cpdlcType part) (cpdlcArgs part)
    | part <- cpdlcParts cpdlc
    ]

cpdlcMessageP :: P.Parsec Void ByteString CPDLCMessage
cpdlcMessageP = do
  slashP
  void $ P.string "data2"
  slashP
  cpdlcMIN <- P.decimal
  slashP
  cpdlcMRN <- P.optional P.decimal
  slashP
  ra <- raP
  slashP
  payloadRaw <- P.takeWhileP Nothing (const True)
  case listToMaybe (parseMessage (Just ra) allMessageTypes payloadRaw) of
    Just partsRaw ->
      return
        CPDLCMessage
          { cpdlcMIN
          , cpdlcMRN
          , cpdlcReplyOpts = ra
          , cpdlcParts = [ CPDLCPart ty args | (ty, args) <- partsRaw ]
          }
    Nothing ->
      fail "Invalid CPDLC message"

raP :: P.Parsec Void ByteString ReplyOpts
raP =
  P.choice . map P.try $
    [ ReplyN <$ P.string "N"
    , ReplyY <$ P.string "Y"
    , ReplyR <$ P.string "R"
    , ReplyAN <$ P.string "AN"
    , ReplyWU <$ P.string "WU"
    ]

raToBS :: ReplyOpts -> ByteString
raToBS ReplyN = "N"
raToBS ReplyY = "Y"
raToBS ReplyR = "R"
raToBS ReplyAN = "AN"
raToBS ReplyWU = "WU"

slashP :: P.Parsec Void ByteString ()
slashP = void $ P.char (ord8 '/')
