module Web.Hoppie.TUI.MCDU.Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views

mcduMain :: TChan MCDUEvent -> MCDU ()
mcduMain eventChan = do
  loadView mainMenuView
  flushAll
  forever $ do
    ev <- liftIO . atomically $ readTChan eventChan
    handleMCDUEvent mainMenuView dlkMenuView atcMenuView ev

