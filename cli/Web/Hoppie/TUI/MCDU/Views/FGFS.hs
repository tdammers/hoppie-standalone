{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
( navView
, perfInitView
, progView
, rteView
, fplView
)
where

import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.MCDU.Views.FGFS.PerfInit
import Web.Hoppie.TUI.MCDU.Views.FGFS.Progress
import Web.Hoppie.TUI.MCDU.Views.FGFS.RouteFpl

import qualified Data.Map.Strict as Map

navView :: MCDUView
navView = defView
  { mcduViewTitle = "NAV MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("DIRECT TO", loadDirectToView Nothing))
      , (LSKL 3, ("NAV INIT", scratchWarn "NOT IMPLEMENTED"))
      , (LSKL 4, ("DEPARTURE", loadView departureView))
      , (LSKR 4, ("ARRIVAL", loadView arrivalView))
      , (LSKL 5, ("MENU", loadViewByID MainMenuView))
      ]
  }

