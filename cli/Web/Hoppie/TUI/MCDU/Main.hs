module Web.Hoppie.TUI.MCDU.Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe

import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.MCDU.Views

mcduMain :: TChan MCDUEvent -> MCDU ()
mcduMain eventChan = do
  modify $ \s -> s { mcduResolveViewID = defResolveViewID }
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
        if headless && ev /= Just TickEvent then
          return Nothing
        else
          return ev
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
    mapM_ (handleMCDUEvent mainMenuView dlkMenuView atcMenuView) evs

mapHttpEvent :: HttpServerEvent -> MCDUEvent
mapHttpEvent (HttpInputCommand cmd) = InputCommandEvent cmd
mapHttpEvent (HttpLogEvent msg) = LogEvent msg
