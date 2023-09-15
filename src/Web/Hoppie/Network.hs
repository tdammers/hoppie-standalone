{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Network
where

import Data.ByteString (ByteString)
import qualified Network.HTTP.Simple as HTTP

data Config =
  Config
    { configLogon ::ByteString
    , configURL :: String
    }
    deriving (Show)

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
          [ ("logon", Just (configLogon config))
          , ("from", Just (requestFrom rq))
          , ("to", Just (requestTo rq))
          , ("type", Just (requestTypeBS $ requestType rq))
          , ("packet", Just (requestPacket rq))
          ]
        <$> HTTP.parseRequest (configURL config)
  rp <- HTTP.httpBS httpRq
  return $ HTTP.getResponseBody rp
