{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS.Progress
where

import Web.Hoppie.FGFS.FMS as FMS
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.MCDU.Views.FGFS.Common
import Web.Hoppie.TUI.StringUtil

import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Encoding
import Text.Printf


formatFlightPhase :: FlightPhase -> ByteString
formatFlightPhase =
  BS8.pack . map (\case { '_' -> ' '; c -> c }) . show

progView :: MCDUView
progView = defView
  { mcduViewTitle = "PROGRESS"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = progViewLoad
  , mcduViewNumPages = 2
  }

progViewLoad :: MCDU ()
progViewLoad = withFGView $ do
  curPage <- gets (mcduViewPage . mcduView)
  progress <- getProgressInfo
  massUnit <- gets mcduMassUnit
  let massFactor = case massUnit of
                      Kilograms -> 1
                      Pounds -> 1 / lbs2kg
      massUnitStr = case massUnit of
                      Kilograms -> "KG"
                      Pounds -> "LBS"
  perfInit <- getPerfInitData
  fuelCapacity <- (* massFactor) <$> getFuelCapacity
  let finres = maybe 0 (* massFactor) $ perfInitReserveFuel perfInit
      cont = maybe 0 (* massFactor) $ perfInitContingencyFuel perfInit
      curPhase = progressFlightPhase <$> progress
      nextPhase = nextFlightPhase . progressFlightPhase <$> progress
      prevPhase = prevFlightPhase . progressFlightPhase =<< progress
  case curPage of
    0 ->
      modifyView $ \v -> v
        { mcduViewDraw = do
            let printWP color y (Just wp) = do
                  mcduPrint 0 y color (encodeUtf8 . Text.take 7 . Text.replace "-" "" $ legName wp)
                  forM_ (legETE wp) $ \ete ->
                    mcduPrint 13 y color (BS8.pack $ formatETE ete)
                  forM_ (legRemainingDist wp) $ \dist ->
                    mcduPrintR 12 y color (BS8.pack $ formatDistanceCompact dist)
                  mcduPrintColoredR 24 y (formatEFOB color finres cont fuelCapacity ((* massFactor) <$> legEFOB wp))
                printWP color y Nothing = do
                  mcduPrint 0 y color "-----"
                  mcduPrintColoredR 24 y (formatEFOB color finres cont fuelCapacity Nothing)

            mcduPrint 1 1 white "TO"
            mcduPrint 8 1 white "DIST"
            mcduPrint 14 1 white "ETE"
            mcduPrint 20 1 white "FUEL"
            printWP magenta 2 (progressCurrent =<< progress)

            mcduPrint 1 3 white "NEXT"
            printWP green 4 (progressNext =<< progress)

            mcduPrint 1 5 white "DEST"
            printWP green 6 (progressDestination =<< progress)

            forM_ curPhase $ \phase -> do
              when (phase < CRUISE) $
                forM_ (progressDistToTOC =<< progress) $ \toc -> do
                  mcduPrint 1 7 white "TOP OF CLIMB"
                  printWP cyan 8 (Just toc)
              when (phase >= CRUISE) $
                forM_ (progressDistToTOD =<< progress) $ \tod -> do
                  mcduPrint 1 7 white "TOP OF DESCENT"
                  printWP cyan 8 (Just tod)

            forM_ (listToMaybe . progressAlerts =<< progress) $ \alert -> do
              mcduPrintC (screenW `div` 2) 9 red alert

            let rnpColor = case ( progress >>= progressRNP >>= rnpRNP
                                , progress >>= progressRNP >>= rnpANP
                                ) of
                              (Just rnp, Just anp) | rnp < anp -> red
                              _ -> green
            mcduPrint 1 10 rnpColor $ fromMaybe "-----" (progress >>= progressRNP >>= rnpSensorName)
            mcduPrint 7 10 white "RNP-"
            mcduPrint 11 10 rnpColor $ maybe "-.--" (BS8.pack . printf "%3.2f") (progress >>= progressRNP >>= rnpRNP)
            mcduPrint 16 10 white "EPU-"
            mcduPrint 20 10 rnpColor $ maybe "-.--" (BS8.pack . printf "%3.2f") (progress >>= progressRNP >>= rnpANP)

        , mcduViewLSKBindings = Map.fromList $
            []
        }
    1 ->
      modifyView $ \v -> v
        { mcduViewDraw = do
            mcduPrint 1 4 white "FOB"
            mcduPrintColoredR (screenW - 1) 4 $
              colorize green
              ( maybe
                  "-----"
                  (\fob -> 
                    let fobU = floor (fob * massFactor) :: Int
                    in BS8.pack $ printf "%5i" fobU
                  )
                  (progressFOB =<< progress)
              ) <>
              " " <>
              colorize white massUnitStr
            mcduPrint 1 6 white "GROSS WGT"
            mcduPrintColoredR (screenW - 1) 6 $
              colorize green
              ( maybe
                  "-----"
                  (\gw -> 
                    let gwU = floor (gw * massFactor) :: Int
                    in BS8.pack $ printf "%5i" gwU
                  )
                  (progressGrossWeight =<< progress)
              ) <>
              " " <>
              colorize white massUnitStr

            mcduPrintC (screenW `div` 2) 1 white (maybe "-------" formatFlightPhase curPhase)
        , mcduViewLSKBindings = Map.fromList $
            [ (LSKL 0, (colorize white $ formatFlightPhase p, setFlightPhase p >> reloadView))
            | p <- maybeToList prevPhase
            ] ++
            [ (LSKR 0, (colorize white $ formatFlightPhase p, setFlightPhase p >> reloadView))
            | p <- maybeToList nextPhase
            ]

        }
    _ ->
      modifyView $ \v -> v
        { mcduViewDraw = return ()
        , mcduViewLSKBindings = mempty
        }
      

