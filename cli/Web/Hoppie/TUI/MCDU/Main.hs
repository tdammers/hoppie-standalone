module Web.Hoppie.TUI.MCDU.Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe
import System.IO

import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.MCDU.Views

mcduMain :: TChan MCDUEvent -> MCDU ()
mcduMain eventChan = do
  gets mcduHeadless >>= liftIO . hSetEcho stdin
  modify $ \s -> s
    { mcduResolveViewID = defResolveViewID
    , mcduEventChan = Just eventChan
    }
  portMay <- gets mcduHttpPort
  when (isJust portMay) mcduStartHttpServer
  fgfsHostMay <- gets mcduFlightgearHostname
  fgfsPortMay <- gets mcduFlightgearPort
  when (isJust fgfsHostMay && isJust fgfsPortMay) mcduConnectFlightgear
  loadView mainMenuView
  flushAll
  forever $ do
    serverMay <- gets mcduHttpServer
    headless <- gets mcduHeadless
    evs <- liftIO . atomically $ do
      localMay <- do
        ev <- tryReadTChan eventChan
        case (headless, ev) of
          (True, Just KeyEvent {}) -> return Nothing
          _ -> return ev
      webMay <- case serverMay of
        Nothing ->
          return Nothing
        Just server -> do
          fmap mapHttpEvent <$> tryReadTChan (mcduHttpInputChan server)
      case (localMay, webMay) of
        (Just a, Just b) -> return [a, b]
        (Just a, Nothing) -> return [a]
        (Nothing, Just b) -> return [b]
        (Nothing, Nothing) -> retry
    mapM_ handleMCDUEvent evs

mapHttpEvent :: HttpServerEvent -> MCDUEvent
mapHttpEvent (HttpInputCommand cmd) = KeyEvent cmd
mapHttpEvent (HttpLogEvent msg) = LogEvent msg
