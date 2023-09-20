{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.TUI.MCDU.HttpServer
where

import Paths_hoppie_standalone

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.Input

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Web.Twain
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as Vector
import Text.Printf
import Data.Char
import System.FilePath

data MCDUHttpServer =
  MCDUHttpServer
    { mcduHttpThread :: Async ()
    , mcduHttpScreenBufVar :: MVar MCDUScreenBuffer
    , mcduHttpInputChan :: TChan InputCommand
    }

startHttpServer :: Int -> IO MCDUHttpServer
startHttpServer port = do
  inputChan <- newTChanIO
  screenBufVar <- newMVar emptyMCDUScreenBuffer
  thread <- async $ Warp.run port $
              foldr ($)
                (notFound notFoundPage)
                [ get "/" getIndex
                , get "/mcdu.js" $ getStaticFile "application/javascript" "mcdu.js"
                , post "/key" $ postKey screenBufVar inputChan
                , get "/screen" $ getScreen screenBufVar
                ]
  return $ MCDUHttpServer thread screenBufVar inputChan

stopHttpServer :: MCDUHttpServer -> IO ()
stopHttpServer s =
  cancel $ mcduHttpThread s

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
  send $ withHeader ("Content-type", contentType) $ html $ body

getScreen :: MVar MCDUScreenBuffer -> ResponderM a
getScreen screenBufVar = do
  buf <- liftIO $ takeMVar screenBufVar
  send $ text $ serializeScreenBuf buf

serializeScreenBuf :: MCDUScreenBuffer -> Text
serializeScreenBuf (MCDUScreenBuffer cells) =
  Vector.foldl (<>) "" . Vector.map serializeCell $ cells
  where
    serializeCell :: MCDUCell -> Text
    serializeCell (MCDUCell color char) =
      Text.pack $ printf "%02x%c" color (chr $ fromIntegral char)
  
postKey :: MVar MCDUScreenBuffer -> (TChan InputCommand) -> ResponderM a
postKey screenBufVar inputChan = do
  k <- param "key"
  success <- liftIO $ handleKey k
  if success then
    getScreen screenBufVar
  else
    send $ text $ "INVALID"
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
    handleKey _ = return False
