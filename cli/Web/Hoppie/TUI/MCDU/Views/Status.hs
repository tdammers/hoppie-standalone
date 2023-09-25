{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Status
where

import Web.Hoppie.System
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.TUI.MCDU.Views.Enum

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map

statusView :: MCDUView
statusView = defView
  { mcduViewTitle = "STATUS INFO"
  , mcduViewOnLoad = do
      config <- asks hoppieNetworkConfig
      callsign <- lift getCallsign
      actype <- gets mcduAircraftType
      networkStatus <- gets mcduNetworkStatus
      let networkStatusCBS = case networkStatus of
            NetworkOK -> colorize green "CONNECTED"
            NetworkError str -> lineJoin
              [ colorize red "ERROR:"
              , colorize yellow $ BS8.pack str
              ]
      let infoLines = lineSplit . lineJoin $
            [ colorize white "CALLSIGN: " <> colorize green callsign
            , colorize white "AIRCRAFT: " <> maybe (colorize red "N/A") (colorize green) actype
            , colorize white "LOGON: " <> colorize green (configLogon config)
            , colorize white "URL: " <> colorize green (BS8.pack $ configURL config)
            , colorize white "POLLING: " <>
                colorize green (BS8.pack . show $ configPollingInterval config) <>
                colorize green "s" <>
                colorize white " / " <>
                colorize green (BS8.pack . show $ configFastPollingInterval config) <>
                colorize green "s"
            , colorize white "NETWORK: " <>
                networkStatusCBS
            ]
      let lns = lineWrap screenW . lineJoin $ infoLines
          linesPerPage = 9
          numPages = (length lns + linesPerPage) `div` linesPerPage
      curPage <- gets (min (numPages - 1) . mcduViewPage . mcduView)
      let curLns = take linesPerPage . drop (curPage * linesPerPage) $ lns
      modifyView $ \v -> v
        { mcduViewDraw = zipWithM_ (\n cbs -> mcduPrintColored 0 (n + 1) cbs) [0,1..] curLns
        , mcduViewNumPages = numPages
        , mcduViewLSKBindings = Map.fromList
            [ (4, ("MAIN MENU", loadViewByID MainMenuView))
            ]
        }
  }

