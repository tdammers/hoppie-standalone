{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.Response
( Response (..)
, Message (..)
, MessageType (..)
, parseResponse
, TypedMessage (..)
, TypedPayload (..)
, typedPayloadTypeBS
, toTypedUplink
, toTypedResponse
, toUntypedRequest
)
where

import Web.Hoppie.Telex
import Web.Hoppie.CPDLC.Message
import qualified Web.Hoppie.Network as Network

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P (decimal)
import Data.Void (Void)
import Data.Char
import Data.Bifunctor
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import Text.Read (readMaybe)

ucfirst :: String -> String
ucfirst [] = []
ucfirst (x:xs) = toUpper x : xs

parseResponse :: ByteString -> Either String Response
parseResponse src =
  first P.errorBundlePretty $ P.parse (responseP <* P.eof) "input" src

data MessageType
  = Progress
  | Cpdlc
  | Telex
  | Pong
  | Posrep
  | Position
  | Data
  | Info
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToJSON MessageType where
  toJSON = toJSON . map toLower . show

instance FromJSON MessageType where
  parseJSON j = maybe (fail "MessageType") return . readMaybe . ucfirst =<< parseJSON j

messageTypeCode :: MessageType -> ByteString
messageTypeCode = BS8.pack . map toLower . show

data Response
  = Response ![Message]
  | ErrorResponse !ByteString
  deriving (Show)

data TypedMessage =
  TypedMessage
    { typedMessageID :: Maybe Word
    , typedMessageCallsign :: ByteString
    , typedMessagePayload :: TypedPayload
    }
    deriving (Show, Eq)

instance ToJSON TypedMessage where
  toJSON m =
    JSON.object
      [ "id" .= typedMessageID m
      , "callsign" .= decodeUtf8 (typedMessageCallsign m)
      , "payload" .= typedMessagePayload m
      ]

instance FromJSON TypedMessage where
  parseJSON = JSON.withObject "TypedMessage" $ \obj ->
    TypedMessage
      <$> obj .: "id"
      <*> (encodeUtf8 <$> obj .: "callsign")
      <*> obj .: "payload"

typedPayloadTypeBS :: TypedPayload -> ByteString
typedPayloadTypeBS (TelexPayload {}) = "TELEX"
typedPayloadTypeBS (InfoPayload {}) = "INFO"
typedPayloadTypeBS (CPDLCPayload {}) = "CPDLC"
typedPayloadTypeBS (UnsupportedPayload ty _) = BS8.pack . map toUpper . show $ ty
typedPayloadTypeBS (ErrorPayload {}) = "ERROR"

data TypedPayload
  = TelexPayload !ByteString
  | InfoPayload !ByteString
  | CPDLCPayload !CPDLCMessage
  | UnsupportedPayload !MessageType !ByteString
  | ErrorPayload !(Maybe MessageType) !ByteString !String
  deriving (Show, Eq)

instance ToJSON TypedPayload where
  toJSON (TelexPayload txt) =
    JSON.object
      [ "type" .= ("telex" :: Text)
      , "data" .= decodeUtf8 txt
      ]
  toJSON (InfoPayload txt) =
    JSON.object
      [ "type" .= ("info" :: Text)
      , "data" .= decodeUtf8 txt
      ]
  toJSON (CPDLCPayload cpdlc) =
    JSON.object
      [ "type" .= ("cpdlc" :: Text)
      , "data" .= cpdlc
      ]
  toJSON (UnsupportedPayload ty txt) =
    JSON.object
      [ "type" .= ("unsupported" :: Text)
      , "msg-type" .= ty
      , "data" .= decodeUtf8 txt
      ]
  toJSON (ErrorPayload tyMay msg err) =
    JSON.object
      [ "type" .= ("error" :: Text)
      , "msg-type" .= tyMay
      , "data" .= decodeUtf8 msg
      , "err" .= err
      ]

instance FromJSON TypedPayload where
  parseJSON = JSON.withObject "TypedPayload" $ \obj -> do
    ty :: Text <- obj .: "type"
    case ty of
      "telex" -> TelexPayload <$> (encodeUtf8 <$> obj .: "data")
      "info" -> InfoPayload <$> (encodeUtf8 <$> obj .: "data")
      "cpdlc" -> CPDLCPayload <$> obj .: "data"
      "unsupported" -> UnsupportedPayload
                        <$> obj .: "msg-type"
                        <*> (encodeUtf8 <$> obj .: "data")
      "error" -> ErrorPayload
                        <$> obj .: "msg-type"
                        <*> (encodeUtf8 <$> obj .: "data")
                        <*> obj .: "err"
      _ -> fail "TypedPayload type"

toTypedResponse :: Response -> [TypedMessage]
toTypedResponse (Response msgs) = map toTypedUplink msgs
toTypedResponse (ErrorResponse err) = [TypedMessage Nothing "server" (ErrorPayload Nothing err "Server reported an error")]

toTypedUplink :: Message -> TypedMessage
toTypedUplink msg =
  TypedMessage
    (messageID msg)
    (messageCallsign msg)
    payload
  where
    payload = case messageType msg of
                Telex -> TelexPayload $ telexFilter (messagePayload msg)
                Info -> InfoPayload $ telexFilter (messagePayload msg)
                Cpdlc -> either
                            (ErrorPayload (Just $ messageType msg) (messagePayload msg))
                            CPDLCPayload
                            (parseCPDLCUplink (messagePayload msg))
                ty -> UnsupportedPayload ty (messagePayload msg)

toUntypedRequest :: ByteString -> TypedMessage -> Network.Request
toUntypedRequest sender tm =
  Network.Request
    { Network.requestFrom = sender
    , Network.requestTo = typedMessageCallsign tm
    , Network.requestType = ty
    , Network.requestPacket = payload
    }
  where
    (ty, payload) =
      case typedMessagePayload tm of
        CPDLCPayload cpdlc ->
          (Network.Cpdlc, renderCPDLCMessage cpdlc)
        TelexPayload body ->
          (Network.Telex, body)
        InfoPayload body ->
          (Network.Inforeq, body)
        UnsupportedPayload {} ->
          error "Unsupported payload type"
        ErrorPayload {} ->
          error "Error payload"

data Message =
  Message
    { messageID :: Maybe Word
    , messageCallsign :: ByteString
    , messageType :: MessageType
    , messagePayload :: ByteString
    }
    deriving (Show)

responseP :: P.Parsec Void ByteString Response
responseP =
  P.choice
    [ P.try errorResponseP
    , P.try okResponseP
    ]

errorResponseP :: P.Parsec Void ByteString Response
errorResponseP = do
  void $ P.string "error"
  P.space1
  bracedP $
    ErrorResponse <$> messagePayloadP
    

okResponseP :: P.Parsec Void ByteString Response
okResponseP = do
  void $ P.string "ok"
  P.space1 <|> P.eof
  Response <$>
    P.many messageP

messageP :: P.Parsec Void ByteString Message
messageP =
  bracedP $
    Message
      <$> (P.optional (P.try $ P.decimal <* P.space1))
      <*> barewordP
      <* P.space1
      <*> messageTypeP
      <* P.space1
      <*> bracedP messagePayloadP
      <* P.space

openBraceP :: P.Parsec Void ByteString ()
openBraceP = void $ P.char (ord8 '{')

closeBraceP :: P.Parsec Void ByteString ()
closeBraceP = void $ P.char (ord8 '}')

bracedP :: P.Parsec Void ByteString a -> P.Parsec Void ByteString a
bracedP x = openBraceP *> x <* closeBraceP <* P.space

barewordP :: P.Parsec Void ByteString ByteString
barewordP = P.takeWhile1P (Just "non-space") (not . isSpace8)

messagePayloadP :: P.Parsec Void ByteString ByteString
messagePayloadP = mconcat <$> P.many (bracedP messagePayloadP <|> messageBarePayloadP)

messageBarePayloadP :: P.Parsec Void ByteString ByteString
messageBarePayloadP = P.takeWhile1P (Just "payload") (`notElem` [ord8 '}', ord8 '{'])

messageTypeP :: P.Parsec Void ByteString MessageType
messageTypeP =
  P.choice
    [ mt <$ P.string (messageTypeCode mt)
    | mt <- [minBound .. maxBound]
    ]
