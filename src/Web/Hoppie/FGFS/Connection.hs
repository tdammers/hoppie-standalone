{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Hoppie.FGFS.Connection
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (showDigest, sha256)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.WebSockets as WS
import System.Random

import Web.Hoppie.FGFS.NasalValue

import Paths_hoppie_standalone

{-# ANN type FGFSConnection ("HLint: ignore Use newtype instead of data" :: String) #-}

data FGFSConnection =
  FGFSConnection
    { fgfsBaseURL :: String
    , fgfsOutputPropertyPath :: String
    , fgfsWebsocketConn :: WS.Connection
    , fgfsOutputChan :: TChan ByteString
    , fgfsConnFailed :: TMVar ()
    }

instance Show FGFSConnection where
  show conn = "FGFSConnection " ++ show (fgfsBaseURL conn) ++ " " ++ show (fgfsOutputPropertyPath conn)

loadNasalLibrary :: MonadIO m => FGFSConnection -> Text -> FilePath -> m ()
loadNasalLibrary conn moduleName filePath = liftIO $ do
  src <- Text.readFile =<< getDataFileName filePath
  let hash = Text.pack . showDigest . sha256 . LBS.fromStrict . encodeUtf8 $ src
  let wrappedSrc =
        "globals.externalMCDU.loadModule(" <>
          encodeNasal hash <> ", " <>
          encodeNasal moduleName <>
          ", func (mcdu) { " <> src <>
          "}, 0);"
  runNasal conn wrappedSrc

withFGFSConnection :: String -> Int -> (FGFSConnection -> IO ()) -> IO ()
withFGFSConnection host0 port action = do
  let host = if host0 == "localhost" then "127.0.0.1" else host0
  let url = "http://" ++ host ++ ":" ++ show port
  connID <- mkRandomID
  let path = "/sim/connections/" ++ connID
  outputChan <- newTChanIO
  connFailedVar <- newEmptyTMVarIO
  (WS.runClient host port "/PropertyListener" $ \wsConn -> do
        let runSender = do
                bracket
                  (connect path url wsConn outputChan connFailedVar)
                  disconnect
                  action
            runReader = forever $ do
                value <- WS.receiveData wsConn
                -- print value
                atomically . writeTChan outputChan $ value
        race_ runReader runSender) `finally` (atomically $ putTMVar connFailedVar ())
  where

    connect :: String -> String -> WS.Connection -> TChan ByteString -> TMVar () -> IO FGFSConnection
    connect path url wsConn outputChan connFailedVar = do
      let conn = FGFSConnection url path wsConn outputChan connFailedVar

      -- Load our shim
      shimSrc <- Text.readFile =<< getDataFileName "nasal/shim.nas"
      runNasalVoid conn shimSrc

      -- Create the output property we use to get Nasal values back out of FGFS
      runNasalVoid conn $ "setprop(\"" <> Text.pack path <> "\", '');"
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
      -- runNasalVoid conn $
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

callNasalFunc :: (MonadIO m, ToNasal args, FromNasal ret) => FGFSConnection -> Text -> args -> m ret
callNasalFunc conn funcname args = liftIO $ do
  let nasalArg = case toNasal args of
        NasalVector nasalArgs -> do
          nasalArgs
        NasalNil -> do
          mempty
        x -> throw $ NasalUnexpected "vector" (show x)
  callNasalOrError conn funcname nasalArg >>= \case
    NasalError err ->
      throw err
    NasalValue nval ->
      case fromNasal nval of
        Left err ->
          throw err
        Right val ->
          return val

callNasalOrError :: FGFSConnection -> Text -> Vector NasalValue -> IO NasalValueOrError
callNasalOrError conn fun args = do
  let script' =
        "externalMCDU.callFunction('" <>
          Text.pack (fgfsOutputPropertyPath conn) <> "', " <>
          encodeNasal fun <>
          "," <>
          encodeNasal args  <> ");"
  runNasalVoid conn script'
  getFGOutput conn

runNasalVoid :: FGFSConnection -> Text -> IO ()
runNasalVoid conn script =
  runFGCommand conn (nasalCommand script)

runNasal_ :: MonadIO m => FGFSConnection -> Text -> m ()
runNasal_ conn script = do
  (_ :: NasalValue) <- runNasal conn script
  return ()

runNasal :: MonadIO m => FromNasal a => FGFSConnection -> Text -> m a
runNasal conn script = liftIO $ do
  runNasalOrError conn script >>= \case
    NasalError err ->
      throw err
    NasalValue nval ->
      case fromNasal nval of
        Left err ->
          throw err
        Right val ->
          return val

runNasalOrError :: FGFSConnection -> Text -> IO NasalValueOrError
runNasalOrError conn script = do
  let script' =
        "externalMCDU.runScript('" <>
          Text.pack (fgfsOutputPropertyPath conn) <> "', " <> encodeNasal script <> ");"
  runNasalVoid conn script'
  getFGOutput conn

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

runFGCommand :: FGFSConnection -> FGCommand -> IO ()
runFGCommand conn cmd = do
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

data PropResponse a =
  PropResponse
    { propResponsePath :: Text
    , propResponseName :: Text
    , propResponseValue :: a
    , propResponseType :: Text
    , propResponseIndex :: Int
    , propResponseNChildren :: Int
    }
    deriving (Show)

instance FromJSON a => FromJSON (PropResponse a) where
  parseJSON = JSON.withObject "PropResponse" $ \obj -> do
    (strValue :: Text) <- obj .: "value"
    value <- either fail return $ JSON.eitherDecodeStrict $ encodeUtf8 strValue
    PropResponse
      <$> obj .: "path"
      <*> obj .: "name"
      <*> parseJSON value
      <*> obj .: "type"
      <*> obj .: "index"
      <*> obj .: "nChildren"

data PropJSONError =
  PropJSONDecodeError !String !String
  deriving (Show)

instance Exception PropJSONError where

getFGProp :: forall a. FromJSON a => FGFSConnection -> Text -> IO a
getFGProp conn path = do
  let rqURL = fgfsBaseURL conn <> "/json/" <> Text.unpack path;
  -- putStrLn $ "Requesting " <> rqURL 
  httpRq <- HTTP.parseRequest rqURL
  rp <- HTTP.httpBS httpRq
  let raw = HTTP.getResponseBody rp
  let parsed = either (throw . PropJSONDecodeError (Text.unpack $ decodeUtf8 raw)) id $ JSON.eitherDecodeStrict raw :: PropResponse a
  return $ propResponseValue parsed

getFGOutput :: forall a. FromJSON a => FGFSConnection -> IO a
getFGOutput conn =
  race catchFailed go >>= \case
    Left () -> throw $ PropJSONDecodeError "<EOF>" "Connection closed"
    Right x -> return x
  where
    catchFailed = do
      atomically $ readTMVar (fgfsConnFailed conn)

    go = do
      rawResponse <- atomically $ readTChan $ fgfsOutputChan conn
      case JSON.eitherDecodeStrict rawResponse of
        Left err ->
          throw $ PropJSONDecodeError
            (Text.unpack (decodeUtf8 rawResponse))
            err
        Right r -> do
          return $ propResponseValue r
