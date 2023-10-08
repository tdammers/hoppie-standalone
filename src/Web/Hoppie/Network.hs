{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.Network
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Network.HTTP.Simple (HttpException (..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import Control.Exception
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

data AtisSource
  = AtisSourceHoppie
  | AtisSourceVatsimDatafeed
  deriving (Show)

instance ToJSON AtisSource where
  toJSON AtisSourceHoppie = JSON.String "hoppie"
  toJSON AtisSourceVatsimDatafeed = JSON.String "vatsim"

instance FromJSON AtisSource where
  parseJSON (JSON.String "hoppie") = pure AtisSourceHoppie
  parseJSON (JSON.String "vatsim") = pure AtisSourceVatsimDatafeed
  parseJSON x = JSON.unexpected x

data Config =
  Config
    { configLogon ::ByteString
    , configURL :: String
    , configPollingInterval :: Int
    , configFastPollingInterval :: Int
    , configAtisSource :: AtisSource
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

requestSummary :: HTTP.Request -> String
requestSummary rq =
  "HTTP " ++ BS8.unpack (HTTP.method rq) ++ " " ++ show (HTTP.getUri rq)

formatHttpExceptionContent :: HTTP.HttpExceptionContent -> String
formatHttpExceptionContent (HTTP.StatusCodeException rp body) =
  (show . HTTP.statusCode . HTTP.responseStatus $ rp) ++ " " ++
  (BS8.unpack . HTTP.statusMessage . HTTP.responseStatus $ rp) ++ " " ++
  (BS8.unpack body)
formatHttpExceptionContent HTTP.TooManyRedirects {} = "TOO MANY REDIRECTS"
formatHttpExceptionContent HTTP.OverlongHeaders = "OVERLONG HEADERS"
formatHttpExceptionContent HTTP.ResponseTimeout = "RESPONSE TIMEOUT"
formatHttpExceptionContent HTTP.ConnectionTimeout = "CONNECTION TIMEOUT"
formatHttpExceptionContent (HTTP.ConnectionFailure e) = "CONNECTION FAILURE " ++ show e
formatHttpExceptionContent (HTTP.InvalidStatusLine l) = "INVALID STATUS LINE " ++ BS8.unpack l
formatHttpExceptionContent (HTTP.InvalidHeader h) = "INVALID HEADER " ++ BS8.unpack h
formatHttpExceptionContent (HTTP.InvalidRequestHeader h) = "INVALID REQUEST HEADer " ++ BS8.unpack h
formatHttpExceptionContent (HTTP.InternalException e) = "INTERNAL EXCEPTION " ++ show e
formatHttpExceptionContent (HTTP.ProxyConnectException host port status) =
  "PROXY CONNECT EXCEPTion " ++ BS8.unpack host ++ ":" ++ show port ++ " " ++ 
  (show . HTTP.statusCode $ status) ++ " " ++
  (BS8.unpack . HTTP.statusMessage $ status)
formatHttpExceptionContent HTTP.NoResponseDataReceived = "NO RESPONSE DATA RECeived"
formatHttpExceptionContent HTTP.TlsNotSupported = "TLS NOT SUPPORTED"
formatHttpExceptionContent (HTTP.WrongRequestBodyStreamSize expected actual) =
  "WRONG REQUEST BODY STREAM SIZE - EXPECTED " ++ show expected ++ " BUT GOT " ++ show actual
formatHttpExceptionContent (HTTP.ResponseBodyTooShort expected actual) =
  "RESPONSE BODY TOO SHORT - EXPECTED " ++ show expected ++ " BUT GOT " ++ show actual
formatHttpExceptionContent HTTP.InvalidChunkHeaders = "INVALID CHUNK HEADERS"
formatHttpExceptionContent HTTP.IncompleteHeaders = "INCOMPLETE HEADERS"
formatHttpExceptionContent (HTTP.InvalidDestinationHost host) = "INVALID DESTINATION HOST " ++ BS8.unpack host
formatHttpExceptionContent HTTP.HttpZlibException {} = "HTTP ZLIB EXCEPTION "
formatHttpExceptionContent (HTTP.InvalidProxyEnvironmentVariable name val) =
  "INVALID PROXY ENVIRONMENT VARIABLE " ++ Text.unpack name ++ "=" ++ Text.unpack val
formatHttpExceptionContent HTTP.ConnectionClosed = "CONNECTION CLOSED"
formatHttpExceptionContent (HTTP.InvalidProxySettings reason) = "INVALID PROXY SETTINGS " ++ Text.unpack reason

sendRequestEither :: Config -> Request -> IO (Either String ByteString)
sendRequestEither config rq =
  (Right <$> sendRequest config rq)
  `catch`
  (\(e :: HttpException) -> case e of
    InvalidUrlException url reason ->
      return . Left $ "INVALID URL: " ++ url ++ " " ++ reason
    HttpExceptionRequest errRQ content ->
      return . Left $ "HTTP ERROR\n----------\n" ++ formatHttpExceptionContent content ++ "\n--- REQUEST: ---\n" ++ requestSummary errRQ
  )
  -- `catch`
  -- (\(e :: SomeException) -> return $ Left $ show e)

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
