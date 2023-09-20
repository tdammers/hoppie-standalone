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
  portMay <- gets mcduHttpPort
  when (isJust portMay) mcduStartHttpServer
  loadView mainMenuView
  flushAll
  forever $ do
    serverMay <- gets mcduHttpServer
    evs <- liftIO . atomically $ do
      localMay <- tryReadTChan eventChan
      webMay <- case serverMay of
        Nothing ->
          return Nothing
        Just server ->
          fmap InputCommandEvent <$> tryReadTChan (mcduHttpInputChan server)
      case (localMay, webMay) of
        (Just a, Just b) -> return [a, b]
        (Just a, Nothing) -> return [a]
        (Nothing, Just b) -> return [b]
        (Nothing, Nothing) -> retry
    mapM_ (handleMCDUEvent mainMenuView dlkMenuView atcMenuView) evs
