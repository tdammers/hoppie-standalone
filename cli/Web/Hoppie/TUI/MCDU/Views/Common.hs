{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Common
where

import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations

import Control.Monad.State

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

loadUplinkLSK :: LSK -> MCDU ()
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

paginateWithHeadroom :: Int
                     -> Int
                     -> Int
                     -> [a]
                     -> (Int, [a])
paginateWithHeadroom headroom itemsPerPage page items =
  let pageItems = take itemsPerPage . drop (itemsPerPage * page) $ items
      numPages = (length items + headroom + itemsPerPage - 1) `div` itemsPerPage
  in (numPages, pageItems)

paginate :: Int
         -> Int
         -> [a]
         -> (Int, [a])
paginate = paginateWithHeadroom 0
