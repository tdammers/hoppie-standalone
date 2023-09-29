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
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (showDigest, sha256)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import System.Random
import Data.Time
import Text.Printf
import System.Socket (Socket)
import qualified System.Socket as Socket
import System.Socket.Family.Inet as Socket
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import Data.Word

import Web.Hoppie.FGFS.NasalValue

import Paths_hoppie_standalone

{-# ANN type FGFSConnection ("HLint: ignore Use newtype instead of data" :: String) #-}

data FGFSConnection =
  FGFSConnection
    { fgfsHostname :: ByteString
    , fgfsPort :: Int
    , fgfsSocket :: Socket Inet Stream TCP
    , fgfsOutputChan :: TChan Word8
    , fgfsConnFailed :: TMVar ()
    , fgfsLogger :: String -> IO ()
    }

instance Show FGFSConnection where
  show conn = printf "FGFSConnection %s:%i" (decodeUtf8 $ fgfsHostname conn) (fgfsPort conn)

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

withFGFSConnection :: ByteString -> Int -> (String -> IO ()) -> (FGFSConnection -> IO ()) -> IO ()
withFGFSConnection host port logger action = do
  connFailedVar <- newEmptyTMVarIO
  outputChan <- newTChanIO
  connVar <- newEmptyTMVarIO

  (do
    let runSender = do
            bracket
              (connect outputChan connFailedVar)
              disconnect
              (\conn -> do
                 atomically $ putTMVar connVar conn
                 putStrLn "Load shim..."
                 shimSrc <- Text.readFile =<< getDataFileName "nasal/shim.nas"
                 void $ runNasalRaw conn shimSrc
                 putStrLn "Loaded shim"
                 loadNasalLibrary conn "fms" "nasal/flightplan.nas"
                 putStrLn "Loaded FMS library"
                 action conn
              )
        runReader = do
            sock <- fgfsSocket <$> atomically (readTMVar connVar)
            value <- Socket.receive sock 4096 Socket.msgNoSignal
            case BS.unpack value of
              [] -> do
                putStrLn "End of input"
                return ()
              xs -> do
                atomically $
                  mapM_ (writeTChan outputChan) xs
                runReader
    race_ runReader runSender) `finally` atomically (putTMVar connFailedVar ())
  where

    connect :: TChan Word8 -> TMVar () -> IO FGFSConnection
    connect outputChan connFailedVar = do
      addrInfos :: [Socket.AddressInfo Inet Stream TCP] <-
          Socket.getAddressInfo (Just host) (Just $ BS8.pack $ show port) mempty
      addr <- case addrInfos of
        [] -> error $ "DNS failure: host " ++ show host ++ " not found."
        (x:_) -> return (Socket.socketAddress x)
      print addr
      sock <- Socket.socket
      Socket.connect sock addr
      let conn = FGFSConnection host port sock outputChan connFailedVar logger
      _ <- Socket.send sock "data\r\n" Socket.msgNoSignal
      putStrLn "connected"
      return conn

    disconnect :: FGFSConnection -> IO ()
    disconnect conn = do
      Socket.close (fgfsSocket conn)

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
callNasalOrError conn fun args = 
  logTime ("callNasalOrError " <> Text.unpack fun) conn $ \_ -> do
    let script' = "externalMCDU.callFunction(" <> encodeNasal fun <> "," <> encodeNasal args  <> ");"
    json <- runNasalRaw conn script'
    case JSON.eitherDecodeStrict json of
      Left err ->
        throw $ JSONDecodeError err
      Right val ->
        return val

getRawResponse :: FGFSConnection -> IO ByteString
getRawResponse conn =
  go ""
  where
    chan = fgfsOutputChan conn

    go bs = do
      b <- atomically $ readTChan chan
      -- print b
      let bs' = BS.snoc bs b
      if b == 10 then do
        return $ BS.snoc bs' b
      else
        go bs'

data FGFSNetworkError
  = FGFSEndOfStream
  deriving (Show)

instance Exception FGFSNetworkError where

runNasalRaw :: FGFSConnection -> Text -> IO ByteString
runNasalRaw conn script = do
  send $ "nasal\r\n" <> encodeUtf8 script <> "\r\n##EOF##\r\n"
  getRawResponse conn
  where
    send bs = do
      sent <- Socket.send (fgfsSocket conn) (BS.take 1024 bs) Socket.msgNoSignal
      when (sent == 0) (throw FGFSEndOfStream)
      when (sent < BS.length bs) $ do
        send (BS.drop sent bs)

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

{-# ANN runNasalOrError ("HLint: ignore Redundant <$>" :: String) #-}

runNasalOrError :: FGFSConnection -> Text -> IO NasalValueOrError
runNasalOrError conn script = do
  let hash = Text.pack . showDigest . sha256 . LBS.fromStrict . encodeUtf8 $ script
      uniq = encodeNasal hash
  let scriptChunks = Text.chunksOf 4096 script

  let script'begin = "externalMCDU.beginScript(" <> uniq <> ");"
  void $ runNasalRaw conn script'begin

  forM_ scriptChunks $ \chunk -> do
    let script'chunk = "externalMCDU.pushScriptCode(" <> uniq <> ", " <> encodeNasal chunk <> ");"
    void $ runNasalRaw conn script'chunk

  let script'finish = "externalMCDU.finishScript(" <> uniq <> ");"
  (JSON.eitherDecodeStrict <$> runNasalRaw conn script'finish) >>= \case
    Left err -> throw $ JSONDecodeError err
    Right a -> return a

newtype JSONError = JSONDecodeError String
  deriving (Show)

instance Exception JSONError where

logTime :: String -> FGFSConnection -> (FGFSConnection -> IO a) -> IO a
logTime label conn action = do
  let logger :: String -> IO ()
      logger = fgfsLogger conn

  startTime <- getCurrentTime
  logger $ printf "%s: %s" (formatTime defaultTimeLocale "%FT%T%Q%z" startTime) label
  retval <- action conn
  endTime <- getCurrentTime
  let timeDiff = diffUTCTime endTime startTime
  logger $ printf "%s spent in %s" (formatTime defaultTimeLocale "%12Es" timeDiff) label
  return retval
