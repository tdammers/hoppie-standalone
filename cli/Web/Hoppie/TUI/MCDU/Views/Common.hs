{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Common
where

import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.MCDU.Monad

import Control.Monad.State

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

loadUplinkLSK :: Int -> MCDU ()
loadUplinkLSK lsk = do
  unreadDLK <- gets mcduUnreadDLK
  unreadCPDLC <- gets mcduUnreadCPDLC
  case unreadCPDLC of
    Just cpdlcUID ->
      addLskBinding lsk "ATC UPLINK" $
        loadViewByID (MessageView cpdlcUID)
    Nothing ->
      case unreadDLK of
        Just dlkUID ->
          addLskBinding lsk "DLK UPLINK" $
            loadViewByID (MessageView dlkUID)
        Nothing ->
          removeLskBinding lsk
