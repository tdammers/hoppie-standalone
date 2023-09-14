{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Response
( PollResponse (..)
, Message (..)
, parsePollResponse
, TypedMessage (..)
, TypedPayload (..)
, toTypedMessage
, toTypedResponse
)
where

import Web.Hoppie.Telex

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

parsePollResponse :: ByteString -> Either String PollResponse
parsePollResponse src =
  first P.errorBundlePretty $ P.parse (pollResponseP <* P.eof) "input" src

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

messageTypeCode :: MessageType -> ByteString
messageTypeCode = BS8.pack . map toLower . show

newtype PollResponse =
  PollResponse
    { pollMessages :: [Message]
    }
    deriving (Show)

data TypedMessage =
  TypedMessage
    { typedMessageID :: Maybe Word
    , typedMessageCallsign :: ByteString
    , typedMessagePayload :: TypedPayload
    }
    deriving (Show)

data TypedPayload
  = TelexPayload !ByteString
  | InfoPayload !ByteString
  | UnsupportedPayload !MessageType !ByteString
  deriving (Show)

toTypedResponse :: PollResponse -> [TypedMessage]
toTypedResponse (PollResponse msgs) = map toTypedMessage msgs

toTypedMessage :: Message -> TypedMessage
toTypedMessage msg =
  TypedMessage
    (messageID msg)
    (messageCallsign msg)
    payload
  where
    payload = case messageType msg of
                Telex -> TelexPayload $ telexFilter (messagePayload msg)
                Info -> InfoPayload $ telexFilter (messagePayload msg)
                ty -> UnsupportedPayload ty (messagePayload msg)

data Message =
  Message
    { messageID :: Maybe Word
    , messageCallsign :: ByteString
    , messageType :: MessageType
    , messagePayload :: ByteString
    }
    deriving (Show)

pollResponseP :: P.Parsec Void ByteString PollResponse
pollResponseP = do
  void $ P.string "ok"
  P.space1
  PollResponse <$>
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
