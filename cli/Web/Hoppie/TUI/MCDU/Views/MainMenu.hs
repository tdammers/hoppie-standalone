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
      fgfsEnabled <- gets (isJust . mcduFlightgearConnection)
      modifyView $ \v -> v
        { mcduViewLSKBindings = Map.fromList $ case curPage of
            0 ->
              [ (LSKL 0, ("NAV", loadViewByID NavView )) | fgfsEnabled ] ++
              [ (LSKL 1, ("FPL", loadViewByID FPLView )) | fgfsEnabled ] ++
              [ (LSKL 2, ("RTE", loadViewByID RTEView )) | fgfsEnabled ] ++
              [ (LSKR 0, ("DLK", loadViewByID DLKMenuView))
              , (LSKR 1, ("ATC", loadViewByID ATCMenuView))
              ]
            1 ->
              [ (LSKL 0, ("CONFIG", loadViewByID ConfigView))
              , (LSKL 1, ("STATUS", loadViewByID StatusView))
              ]
            _ ->
              []
        }
      loadUplinkLSK (LSKR 5)
  }
