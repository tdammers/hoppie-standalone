{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.TUI.MCDU.HttpServer
where

import Paths_hoppie_standalone

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.Input

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Web.Twain
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets as WS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import System.FilePath
import qualified Data.Aeson as JSON
import Control.Monad.IO.Class

data MCDUHttpServer =
  MCDUHttpServer
    { mcduHttpThread :: Async ()
    , mcduHttpScreenBufVar :: MVar MCDUScreenBuffer
    , mcduHttpScreenBufChan :: TChan MCDUScreenBufferUpdate
    , mcduHttpInputChan :: TChan InputCommand
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
         -> TChan InputCommand
         -> Application
httpMain screenBufVar screenBufChan inputChan = 
  foldr ($)
    (notFound notFoundPage)
    [ get "/" getIndex
    , get "/mcdu.js" $ getStaticFile "application/javascript" "mcdu.js"
    , post "/key" $ postKey inputChan
    , get "/screen" $ getScreen screenBufVar
    , wsMain
    ]
  where
    wsMain :: Middleware
    wsMain parent rq =
      if pathInfo rq == ["screen", "updates"] then
        WS.websocketsOr WS.defaultConnectionOptions wsApp parent rq
      else
        parent rq
        
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      conn <- WS.acceptRequest pendingConn
      chan <- atomically $ dupTChan screenBufChan
      forever $ do
        update <- atomically $ readTChan chan
        WS.sendTextData conn (JSON.encode update)

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
  send $ raw status200 [("Content-type", contentType)] body

getScreen :: MVar MCDUScreenBuffer -> ResponderM a
getScreen screenBufVar = do
  buf <- liftIO $ readMVar screenBufVar
  send $ json buf

postKey :: TChan InputCommand -> ResponderM a
postKey inputChan = do
  k <- param "key"
  success <- liftIO $ handleKey k
  if success then
    send $ text "OK"
  else
    send $ status status400 $ text "INVALID"
  where
    handleInput = liftIO . atomically . writeTChan inputChan

    handleKey [c]
      | isAlphaNum c || c `elem` ['.', ' ', '-', '/']
      = handleInput (InputChar $ toUpper c) >> return True
    handleKey "F1" = handleInput InputF1 >> return True
    handleKey "F2" = handleInput InputF2 >> return True
    handleKey "F3" = handleInput InputF3 >> return True
    handleKey "F4" = handleInput InputF4 >> return True
    handleKey "F5" = handleInput InputF5 >> return True
    handleKey "F6" = handleInput InputF6 >> return True
    handleKey "F7" = handleInput InputF7 >> return True
    handleKey "F8" = handleInput InputF8 >> return True
    handleKey "F9" = handleInput InputF9 >> return True
    handleKey "F10" = handleInput InputF10 >> return True
    handleKey "F11" = handleInput InputF11 >> return True
    handleKey "F12" = handleInput InputF12 >> return True
    handleKey "MENU" = handleInput InputEscape >> return True
    handleKey "PGUP" = handleInput InputPgUp >> return True
    handleKey "PGDN" = handleInput InputPgDn >> return True
    handleKey "BACK" = handleInput InputBackspace >> return True
    handleKey "DEL" = handleInput InputDel >> return True
    handleKey "ESC" = handleInput InputEscape >> return True
    handleKey _ = return False
