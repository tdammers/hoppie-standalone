{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.FGFS.Connection
where

import Control.Monad
import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as JSON
import System.Random
import qualified Data.Map.Strict as Map
import Control.Exception
import Data.Maybe
import qualified Network.WebSockets as WS
import Data.Text.Encoding
import Control.Concurrent.STM
import Control.Concurrent.Async

import Paths_hoppie_standalone

{-# ANN type FGFSConnection ("HLint: ignore Use newtype instead of data" :: String) #-}

data FGFSConnection =
  FGFSConnection
    { fgfsBaseURL :: String
    , fgfsOutputPropertyPath :: String
    , fgfsWebsocketConn :: WS.Connection
    , fgfsOutputChan :: TChan ByteString
    }

withFGFSConnection :: String -> Int -> (FGFSConnection -> IO a) -> IO a
withFGFSConnection host0 port action = do
  let host = if host0 == "localhost" then "127.0.0.1" else host0
  let url = "http://" ++ host ++ ":" ++ show port
  connID <- mkRandomID
  let path = "/sim/connections/" ++ connID
  outputChan <- newTChanIO
  WS.runClient host port "/PropertyListener" $ \wsConn -> do
        let runSender = do
                bracket
                  (connect path url wsConn outputChan)
                  disconnect
                  action
            runReader = forever $ do
                WS.receiveData wsConn >>= atomically . writeTChan outputChan
        snd <$> concurrently runReader runSender 
  where

    connect :: String -> String -> WS.Connection -> TChan ByteString -> IO FGFSConnection
    connect path url wsConn outputChan = do
      let conn = FGFSConnection url path wsConn outputChan

      -- Load our shim
      shimSrc <- Text.readFile =<< getDataFileName "nasal/shim.nas"
      runNasalVoidWS conn shimSrc

      -- Create the output property we use to get Nasal values back out of FGFS
      runNasalVoidWS conn $ "setprop(\"" <> Text.pack path <> "\", '');"
      WS.sendBinaryData wsConn . JSON.encode $
        JSON.object
          [ "command" .= ("addListener" :: Text)
          , "node" .= path
          ]
      return conn

    disconnect :: FGFSConnection -> IO ()
    disconnect conn = do
      let path = fgfsOutputPropertyPath conn
      WS.sendBinaryData (fgfsWebsocketConn conn) . JSON.encode $
        JSON.object
          [ "command" .= ("removeListener" :: Text)
          , "node" .= path
          ]
      -- -- This will crash FG:
      -- runNasalVoidWS conn $
      --     "(func () { " <>
      --     "    var p = props.globals.getNode(\"" <> Text.pack path <> "\");" <>
      --     "    if (p != nil) p.remove();" <>
      --     "})();"
      WS.sendClose (fgfsWebsocketConn conn) ("Bye!" :: Text)

mkRandomID :: IO String
mkRandomID =
  replicateM 16 mkRandomChar

mkRandomChar :: IO Char
mkRandomChar =
  randomRIO ('a', 'z')

data FGCommand =
  FGCommand
    { fgCommand :: ByteString
    , fgCommandArgs :: FGCommandArgs
    }
  deriving (Show)

newtype FGCommandArgs =
  FGCommandArgs { fgCommandArgsList :: [FGCommandArg] }
  deriving newtype (Show)

instance ToJSON FGCommandArgs where
  toJSON args =
    JSON.object
      [ "name" .= ("" :: Text)
      , "children" .= toJSON (fgCommandArgsList args)
      ]

data FGCommandArg
  = FGCommandArg Text Int Text
  deriving (Show)

instance ToJSON FGCommandArg where
  toJSON (FGCommandArg name index path) =
    JSON.object
      [ "name" .= name
      , "index" .= index
      , "value" .= path
      ]

mkCommand :: ByteString -> [(Text, Text)] -> FGCommand
mkCommand cmd argsRaw =
  FGCommand cmd $ FGCommandArgs $ go mempty argsRaw
  where
    go _counters [] = []
    go counters ((name, val) : xs) =
      let n = fromMaybe 0 (Map.lookup name counters)
          counters' = Map.insert name (n + 1) counters
          arg = FGCommandArg name n val
      in arg : go counters' xs

nasalCommand :: Text -> FGCommand
nasalCommand script =
  mkCommand "nasal" [ ("script", script) ]

runNasalVoid :: FGFSConnection -> Text -> IO ()
runNasalVoid conn script =
  runFGCommand conn (nasalCommand script)

runNasalResult :: FGFSConnection -> Text -> IO JSON.Value
runNasalResult conn script = do
  let script' = "setprop(\"" <> Text.pack (fgfsOutputPropertyPath conn) <> "\"," <>
                "(func { " <>
                "var err = []; "<>
                "var result = call(func {" <> script <> "}, [], null, {}, err);" <>
                "return result;" <>
                ")()" <>
                ");"
  runNasalVoid conn script'
  getFGProp conn (Text.pack $ fgfsOutputPropertyPath conn)

runNasalVoidWS :: FGFSConnection -> Text -> IO ()
runNasalVoidWS conn script =
  runFGCommandWS conn (nasalCommand script)

runNasalResultWS :: FGFSConnection -> Text -> IO JSON.Value
runNasalResultWS conn script = do
  let script' =
        "hoppieStandaloneShim.runScript('" <>
          Text.pack (fgfsOutputPropertyPath conn) <> "', func {" <> script <> "});"
  runNasalVoidWS conn script'
  getFGOutputWS conn

runFGCommand :: FGFSConnection -> FGCommand -> IO ()
runFGCommand conn cmd = do
  runFGCommandRaw (fgfsBaseURL conn) cmd

runFGCommandRaw :: String -> FGCommand -> IO ()
runFGCommandRaw baseURL cmd = do
  -- let encoded = JSON.encode (fgCommandArgs cmd)
  httpRq <-
        HTTP.setRequestQueryString
          [ ("value", Just (fgCommand cmd))
          ]
        . HTTP.setRequestMethod "POST"
        . HTTP.setRequestBodyJSON (fgCommandArgs cmd)
        <$> HTTP.parseRequest (baseURL <> "/run.cgi")
  -- BS8.putStrLn $ "HTTP REQUEST: " <> fgCommand cmd <> " " <> LBS.toStrict encoded
  _rp <- HTTP.httpBS httpRq
  -- print $ HTTP.getResponseBody rp
  return ()

runFGCommandWS :: FGFSConnection -> FGCommand -> IO ()
runFGCommandWS conn cmd = do
  let ws = fgfsWebsocketConn conn
      encoded = JSON.encode $
          JSON.object
            [ "command" .= ("exec" :: Text)
            , "fgcommand" .= decodeUtf8 (fgCommand cmd)
            , "children" .= fgCommandArgsList (fgCommandArgs cmd)
            ]
  -- Text.putStrLn $ "WS REQUEST: " <> decodeUtf8 (LBS.toStrict encoded)
  WS.sendBinaryData ws encoded
  -- Text.putStrLn "WS REQUEST SENT"

data PropResponse =
  PropResponse
    { propResponsePath :: Text
    , propResponseName :: Text
    , propResponseValue :: JSON.Value
    , propResponseType :: Text
    , propResponseIndex :: Int
    , propResponseNChildren :: Int
    }
    deriving (Show)

instance FromJSON PropResponse where
  parseJSON = JSON.withObject "PropResponse" $ \obj -> do
    (strValue :: Text) <- obj .: "value"
    value <- either fail return $ JSON.eitherDecodeStrict $ encodeUtf8 strValue
    PropResponse
      <$> obj .: "path"
      <*> obj .: "name"
      <*> pure value
      <*> obj .: "type"
      <*> obj .: "index"
      <*> obj .: "nChildren"

getFGProp :: FGFSConnection -> Text -> IO JSON.Value
getFGProp conn path = do
  let rqURL = fgfsBaseURL conn <> "/json/" <> Text.unpack path;
  -- putStrLn $ "Requesting " <> rqURL 
  httpRq <- HTTP.parseRequest rqURL
  rp <- HTTP.httpBS httpRq
  let raw = HTTP.getResponseBody rp
  let parsed = JSON.decodeStrict raw :: Maybe PropResponse
  return $ maybe "" propResponseValue parsed

getFGOutputWS :: FGFSConnection -> IO JSON.Value
getFGOutputWS conn = do
  rawResponse <- atomically $ readTChan $ fgfsOutputChan conn
  case JSON.decodeStrict rawResponse of
    Nothing ->
      error $ "Invalid response: " ++ Text.unpack (decodeUtf8 rawResponse)
    Just r -> do
      putStrLn "-----"
      print rawResponse
      print r
      return $ propResponseValue r
