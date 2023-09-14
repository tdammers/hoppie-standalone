{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Network
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP
import Data.Word
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P (decimal)
import Data.Void (Void)

data Config =
  Config
    { logon ::ByteString
    , url :: String
    }

defURL :: String
defURL = "http://www.hoppie.nl/acars/system/connect.html"

data RequestType
  = Progress
  | Cpdlc
  | Telex
  | Ping
  | Posreq
  | Position
  | Datareq
  | Inforeq
  | Poll
  | Peek
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

requestTypeBS :: RequestType -> ByteString
requestTypeBS Progress = "progress"
requestTypeBS Cpdlc = "cpdlc"
requestTypeBS Telex = "telex"
requestTypeBS Ping = "ping"
requestTypeBS Posreq = "posreq"
requestTypeBS Position = "position"
requestTypeBS Datareq = "datareq"
requestTypeBS Inforeq = "inforeq"
requestTypeBS Poll = "poll"
requestTypeBS Peek = "peek"

data Request
  = Request
      { requestFrom :: ByteString
      , requestTo :: ByteString
      , requestType :: RequestType
      , requestPacket :: ByteString
      }

sendRequest :: Config -> Request -> IO ByteString
sendRequest config rq = do
  httpRq <-
        HTTP.setRequestQueryString
          [ ("logon", Just (logon config))
          , ("from", Just (requestFrom rq))
          , ("to", Just (requestTo rq))
          , ("type", Just (requestTypeBS $ requestType rq))
          , ("packet", Just (requestPacket rq))
          ]
        <$> HTTP.parseRequest (url config)
  rp <- HTTP.httpBS httpRq
  return $ HTTP.getResponseBody rp

data PollResponse =
  PollResponse
    { pollMessages :: [Message]
    }

data Message =
  Message
    { messageID :: Word
    , messageFrom :: ByteString
    , messageType :: RequestType
    , messageText :: ByteString
    }

messageP :: P.Parsec Void ByteString Message
messageP =
  Message
    <$> P.decimal
    <* P.space1
    <*> barewordP
    <* P.space1
    <*> requestTypeP
    <* P.space
    <*> openBraceP
    <*> messageTextP
    <*> closeBraceP
    <* P.space

barewordP :: P.Parsec Void ByteString ByteString
barewordP = P.manyTill P.any P.space
