{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.MainMenu
where

import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.MCDU.Views.Enum

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe

{-# ANN module ("HLint: ignore Redundant <$>" :: String) #-}

mainMenuView :: MCDUView
mainMenuView = defView
  { mcduViewTitle = "MCDU MENU"
  , mcduViewNumPages = 2
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      fgfsEnabled <-
          (&&)
            <$> gets (isJust . mcduFlightgearHostname)
            <*> gets (isJust . mcduFlightgearPort)
      modifyView $ \v -> v
        { mcduViewLSKBindings = Map.fromList $ case curPage of
            0 ->
              [ (0, ("FPL", return () {- TODO -} )) | fgfsEnabled ] ++
              [ (5, ("DLK", loadViewByID DLKMenuView))
              , (6, ("ATC", loadViewByID ATCMenuView))
              ]
            1 ->
              [ (0, ("CONFIG", loadViewByID ConfigView))
              , (1, ("STATUS", loadViewByID StatusView))
              ]
            _ ->
              []
        }
      loadUplinkLSK 9
  }
