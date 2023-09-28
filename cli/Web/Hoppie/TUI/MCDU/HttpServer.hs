{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.HttpServer
where

import Paths_hoppie_standalone

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Keys

import Prelude hiding (log)

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import System.FilePath
import Web.Twain
import Control.Exception
import Data.Text.Encoding
import Network.Socket
import Text.Printf
import Data.Time

data HttpServerEvent
  = HttpInputCommand MCDUKey
  | HttpLogEvent Text
  deriving (Show, Eq, Ord)

data MCDUHttpServer =
  MCDUHttpServer
    { mcduHttpThread :: Async ()
    , mcduHttpScreenBufVar :: MVar MCDUScreenBuffer
    , mcduHttpScreenBufChan :: TChan MCDUScreenBufferUpdate
    , mcduHttpInputChan :: TChan HttpServerEvent
    }

startHttpServer :: Int -> IO MCDUHttpServer
startHttpServer port = do
  inputChan <- newTChanIO
  screenBufVar <- newMVar emptyMCDUScreenBuffer
  screenBufChan <- newBroadcastTChanIO
  thread <- async $ Warp.run port $ httpMain screenBufVar screenBufChan inputChan
  return $ MCDUHttpServer thread screenBufVar screenBufChan inputChan

stopHttpServer :: MCDUHttpServer -> IO ()
stopHttpServer s =
  cancel $ mcduHttpThread s

httpMain :: MVar MCDUScreenBuffer
         -> TChan MCDUScreenBufferUpdate
         -> TChan HttpServerEvent
         -> Application
httpMain screenBufVar screenBufChan inputChan = 
  foldr ($)
    (notFound notFoundPage)
    [ get "/" getIndex
    , get "/mcdu.js" $ getStaticFile "application/javascript" "mcdu.js"
    , get "/mcdu.css" $ getStaticFile "text/css" "mcdu.css"
    , get "/mcdu.svg" $ getStaticFile "image/svg+xml" "mcdu.svg"
    , post "/key" postKey
    , get "/screen" getScreen
    , wsMain
    ]
  where
    getPeerInfo :: Request -> Text
    getPeerInfo rq =
      let remoteSock = remoteHost rq
      in
        Text.pack $ formatSock remoteSock

    wsMain :: Middleware
    wsMain parent rq = do
      let peerInfo = getPeerInfo rq
      if pathInfo rq == ["websocket"] then
        WS.websocketsOr WS.defaultConnectionOptions (wsApp peerInfo) parent rq
      else
        parent rq
        
    wsApp :: Text -> WS.ServerApp
    wsApp peerInfo pendingConn = do
      let log msg = writeLog peerInfo $ "ws " <> msg

      log "Websocket connection"
      conn <- WS.acceptRequest pendingConn
      chan <- atomically $ dupTChan screenBufChan
      log "Websocket connection accepted"
      let runInputs = forever $ do
            rawCmd <- WS.receiveData conn
            handleKey log rawCmd
      let runOutputs = forever $ do
            update <- atomically $ readTChan chan
            WS.sendTextData conn (JSON.encode update)
      let runAll = do
            race_ runInputs runOutputs
                `catch`
                  (\case 
                    WS.CloseRequest code reason ->
                      log $
                        "Websocket connection closed (" <>
                        Text.pack (show code) <> ": " <>
                        decodeUtf8 (LBS.toStrict reason) <>
                        ")"
                    WS.ConnectionClosed ->
                      log $
                        "Websocket connection closed unexpectedly"
                    WS.ParseException err ->
                      log $
                        "Websocket: parse exception, connection closed: " <> Text.pack err
                    WS.UnicodeException err ->
                      log $
                        "Websocket: unicode error, connection closed: " <> Text.pack err
                  )
                `catch`
                  (\(e :: SomeException) ->
                    log $ "Error while handling websocket connection: " <> (Text.pack . show $ e)
                  )

      WS.withPingThread conn 30 (log "Ping") runAll

    notFoundPage :: ResponderM a
    notFoundPage =
      send $ html $
        "<html>" <>
        "<head><meta charset='utf-8'><title>Web MCDU</title></head>" <>
        "<body><h1>Not found</h1><p>This page does not seem to exist.</p></body>" <>
        "</html>"

    getIndex :: ResponderM a
    getIndex = getStaticFile "text/html" "mcdu.html"

    getStaticFile :: BS.ByteString -> FilePath -> ResponderM a
    getStaticFile contentType filename = do
      path <- liftIO $ getDataFileName ("cli/static" </> filename)
      body <- liftIO $ LBS.fromStrict <$> BS.readFile path
      rq <- request
      liftIO $ logRq rq ("Send file: " <> Text.pack path)
      send $ raw status200 [("Content-type", contentType), ("Content-disposition", "inline")] body

    getScreen :: ResponderM a
    getScreen = do
      rq <- request
      liftIO $ logRq rq "Send screen"
      buf <- liftIO $ readMVar screenBufVar
      send $ json buf

    writeLog :: Text -> Text -> IO ()
    writeLog peerInfo msg = do
      t <- getCurrentTime
      let logLine = Text.pack $
            printf "%s %s %s"
              (formatTime defaultTimeLocale "%FT%T%z" t)
              peerInfo
              msg
      atomically . writeTChan inputChan . HttpLogEvent $ logLine

    handleKey :: (Text -> IO ()) -> Text -> IO Bool
    handleKey log rawKey = do
      log $ "Received message: " <> rawKey
      go . Text.unpack $ rawKey
      where
        handleInput = atomically . writeTChan inputChan . HttpInputCommand

        go [c]
          | isAlphaNum c || c `elem` ['.', ' ', '-', '/']
          = handleInput (MCDUChar $ toUpper c) >> return True
        go "L1" = handleInput (MCDULSK $ LSKL 0) >> return True
        go "L2" = handleInput (MCDULSK $ LSKL 1) >> return True
        go "L3" = handleInput (MCDULSK $ LSKL 2) >> return True
        go "L4" = handleInput (MCDULSK $ LSKL 3) >> return True
        go "L5" = handleInput (MCDULSK $ LSKL 4) >> return True
        go "L6" = handleInput (MCDULSK $ LSKL 5) >> return True
        go "R1" = handleInput (MCDULSK $ LSKR 0) >> return True
        go "R2" = handleInput (MCDULSK $ LSKR 1) >> return True
        go "R3" = handleInput (MCDULSK $ LSKR 2) >> return True
        go "R4" = handleInput (MCDULSK $ LSKR 3) >> return True
        go "R5" = handleInput (MCDULSK $ LSKR 4) >> return True
        go "R6" = handleInput (MCDULSK $ LSKR 5) >> return True

        go "MENU" = handleInput (MCDUFunction Menu) >> return True
        go "PGUP" = handleInput (MCDUFunction PageUp) >> return True
        go "PGDN" = handleInput (MCDUFunction PageDown) >> return True
        go "CLR" = handleInput (MCDUFunction CLR) >> return True
        go "DEL" = handleInput (MCDUFunction DEL) >> return True
        go "DLK" = handleInput (MCDUFunction DLK) >> return True
        go "ATC" = handleInput (MCDUFunction ATC) >> return True
        go "FPL" = handleInput (MCDUFunction FPL) >> return True
        go "RTE" = handleInput (MCDUFunction RTE) >> return True
        go "NAV" = handleInput (MCDUFunction NAV) >> return True
        go _ = return False

    logRq :: Request -> Text -> IO ()
    logRq rq msg = do
      let peerInfo = getPeerInfo rq
          methodStr = Text.pack . show $ requestMethod rq
          path = decodeUtf8 $ rawPathInfo rq
      writeLog peerInfo (Text.unwords [methodStr, path, msg])

    postKey :: ResponderM a
    postKey = do
      rq <- request
      k <- param "key"
      success <- liftIO $ handleKey (logRq rq) k
      if success then
        send $ text "OK"
      else
        send $ status status400 $ text "INVALID"

formatSock :: SockAddr -> String
formatSock (SockAddrInet port addr) =
  let (a, b, c, d) = hostAddressToTuple addr
      p :: Word
      p = fromIntegral port
  in printf "%u.%u.%u.%u:%u" a b c d p
formatSock (SockAddrInet6 port flow addr _) =
  let (a, b, c, d, e, f, g, h) = hostAddress6ToTuple addr
      p :: Word
      p = fromIntegral port
  in printf "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x/%u:%u" a b c d e f g h flow p
formatSock (SockAddrUnix str) =
  str
-- formatSock (SockAddrCan a) =
--   printf "%08x" a
