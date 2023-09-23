{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.Hoppie.FGFS.Connection
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:))
import qualified Data.Aeson as JSON
import System.Random
import qualified Data.Map.Strict as Map
import Control.Exception
import Data.Maybe

{-# ANN type FGFSConnection ("HLint: ignore Use newtype instead of data" :: String) #-}

data FGFSConnection =
  FGFSConnection
    { fgfsBaseURL :: String
    , fgfsOutputPropertyPath :: String
    }
  deriving (Show)

withFGFSConnection :: String -> (FGFSConnection -> IO a) -> IO a
withFGFSConnection url =
  bracket
    (fgfsConnect url)
    fgfsDisconnect

fgfsConnect :: String -> IO FGFSConnection
fgfsConnect url = do
  connID <- mkRandomID
  let path = "/sim/connections/" ++ connID
  let conn = FGFSConnection url path
  runNasalVoid conn $ "setprop(\"" <> Text.pack path <> "\", '');"
  return conn

fgfsDisconnect :: FGFSConnection -> IO ()
fgfsDisconnect conn = do
  let path = fgfsOutputPropertyPath conn
  runNasalVoid conn $
    "(func () { " <>
    "    var p = props.globals.getNode(\"" <> Text.pack path <> "\");" <>
    "    if (p != nil) p.remove();" <>
    "})();"

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
  void $ runFGCommand conn (nasalCommand script)

runNasalResult :: FGFSConnection -> Text -> IO Text
runNasalResult conn script = do
  let script' = "setprop(\"" <> Text.pack (fgfsOutputPropertyPath conn) <> "\"," <>
                    "(func {" <> script <> "})()" <>
                    ");"
  runNasalVoid conn script'
  getFGProp conn (Text.pack $ fgfsOutputPropertyPath conn)

runFGCommand :: FGFSConnection -> FGCommand -> IO ByteString
runFGCommand conn cmd = do
  httpRq <-
        HTTP.setRequestQueryString
          [ ("value", Just (fgCommand cmd))
          ]
        . HTTP.setRequestMethod "POST"
        . HTTP.setRequestBodyJSON (fgCommandArgs cmd)
        <$> HTTP.parseRequest (fgfsBaseURL conn <> "/run.cgi")
  rp <- HTTP.httpBS httpRq
  return $ HTTP.getResponseBody rp

data PropResponse =
  PropResponse
    { propResponsePath :: Text
    , propResponseName :: Text
    , propResponseValue :: Text
    , propResponseType :: Text
    , propResponseIndex :: Int
    , propResponseNChildren :: Int
    }
    deriving (Show)

instance FromJSON PropResponse where
  parseJSON = JSON.withObject "PropResponse" $ \obj ->
    PropResponse
      <$> obj .: "path"
      <*> obj .: "name"
      <*> obj .: "value"
      <*> obj .: "type"
      <*> obj .: "index"
      <*> obj .: "nChildren"

getFGProp :: FGFSConnection -> Text -> IO Text
getFGProp conn path = do
  let rqURL = fgfsBaseURL conn <> "/json/" <> Text.unpack path;
  putStrLn $ "Requesting " <> rqURL 
  httpRq <- HTTP.parseRequest rqURL
  rp <- HTTP.httpBS httpRq
  let raw = HTTP.getResponseBody rp
  -- print raw
  let parsed = JSON.decodeStrict raw :: Maybe PropResponse
  -- maybe (putStrLn "AAAAAAA") print parsed
  return $ maybe "" propResponseValue parsed
