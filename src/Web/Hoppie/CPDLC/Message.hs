{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Hoppie.CPDLC.Message
( CPDLCMessage (..)
, CPDLCPart (..)
, MessageTypeID
, parseCPDLCUplink
, parseCPDLCDownlink
, renderCPDLCMessage
, raToBS
)
where

import Web.Hoppie.CPDLC.MessageTypes
import Web.Hoppie.Telex

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
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

parseCPDLCUplink :: ByteString -> Either String CPDLCMessage
parseCPDLCUplink src =
  first P.errorBundlePretty $ P.parse (cpdlcUplinkP <* P.eof) "input" src

parseCPDLCDownlink :: ByteString -> Either String CPDLCMessage
parseCPDLCDownlink src =
  first P.errorBundlePretty $ P.parse (cpdlcDownlinkP <* P.eof) "input" src

renderCPDLCMessage :: CPDLCMessage -> ByteString
renderCPDLCMessage cpdlc =
  mconcat
    [ "/data2/"
    , (BS8.pack . show) (cpdlcMIN cpdlc)
    , "/"
    , maybe "" (BS8.pack . show) (cpdlcMRN cpdlc)
    , "/"
    , raToBS (cpdlcReplyOpts cpdlc)
    , "/"
    ] <>
  BS8.unwords
    [ renderMessage allMessageTypes (cpdlcType part) (cpdlcArgs part)
    | part <- cpdlcParts cpdlc
    ]

cpdlcUplinkP :: P.Parsec Void ByteString CPDLCMessage
cpdlcUplinkP = cpdlcMessageP uplinkMessages

cpdlcDownlinkP :: P.Parsec Void ByteString CPDLCMessage
cpdlcDownlinkP = cpdlcMessageP uplinkMessages

cpdlcMessageP :: Map MessageTypeID MessageType -> P.Parsec Void ByteString CPDLCMessage
cpdlcMessageP messageTypes = do
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
  case listToMaybe (parseMessage (Just ra) messageTypes payloadRaw) of
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
    [ ReplyN <$ P.string "NE"
    , ReplyN <$ P.string "N"
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
