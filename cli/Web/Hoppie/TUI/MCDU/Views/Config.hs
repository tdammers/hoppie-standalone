{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Config
where

import Web.Hoppie.System
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Enum

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Read (readMaybe)

configView :: MCDUView
configView = defView
  { mcduViewTitle = "CONFIG"
  , mcduViewNumPages = 2
  , mcduViewOnLoad = do
      page <- gets (mcduViewPage . mcduView)

      case page of 
        0 -> do
          actype <- gets mcduAircraftType
          showLog <- gets mcduShowLog
          headless <- gets mcduHeadless
          callsign <- lift getCallsign
          serverEnabled <- gets (isJust . mcduHttpServer)
          let setACType :: Maybe ByteString -> MCDU Bool
              setACType actypeMay =
                True <$ modify (\s -> s { mcduAircraftType = actypeMay })
              getACType :: MCDU (Maybe ByteString)
              getACType =
                gets mcduAircraftType

              setMyCallsign (Just c) = do
                lift $ setCallsign c
                return True
              setMyCallsign Nothing =
                return False
              getMyCallsign =
                lift $ Just <$> getCallsign

          modifyView $ \v -> v
            { mcduViewDraw = do
                mcduPrint 0 2 white "A/C TYPE"
                mcduPrintR (screenW - 1) 2 green (fromMaybe "----" actype)
                mcduPrint 0 4 white "CALLSIGN"
                mcduPrintR (screenW - 1) 4 green callsign
                mcduPrint 0 6 white "SHOW LOG"
                mcduPrintR (screenW - 1) 6 green (if showLog then "ON" else "OFF")
                mcduPrint 0 8 white "HEADLESS MODE"
                if serverEnabled then do
                  mcduPrintR (screenW - 1) 8 green (if headless then "ON" else "OFF")
                else do
                  if headless then
                    mcduPrintR (screenW - 1) 8 red "ON"
                  else
                    mcduPrintR (screenW - 1) 8 yellow "DISABLED"
                  mcduPrint 1 9 white "(HTTP SERVER OFF)"
            , mcduViewLSKBindings = Map.fromList $ 
                [ (4, ("MAIN MENU", loadViewByID MainMenuView))
                , (5, ("", scratchInteract setACType getACType >> reloadView))
                , (6, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                , (7, ("", modify (\s -> s { mcduShowLog = not (mcduShowLog s) }) >> flushAll >> reloadView))
                ]
                ++
                if serverEnabled then
                  [ (8, ("", modify (\s -> s { mcduHeadless = not (mcduHeadless s) }) >> flushAll >> reloadView)) ]
                else
                  [ (8, ("", modify (\s -> s { mcduScratchMessage = Just "NOT ALLOWED" }) >> redrawScratch)) ]
            }
        1 -> do
          serverEnabled <- gets (isJust . mcduHttpServer)
          headless <- gets mcduHeadless
          let setPort :: Maybe ByteString -> MCDU Bool
              setPort portMay =
                case portMay of
                  Nothing ->
                    True <$ modify (\s -> s { mcduHttpPort = Nothing })
                  Just portStr ->
                    case readMaybe . BS8.unpack $ portStr of
                      Nothing -> return False
                      Just port -> do
                        when serverEnabled mcduStopHttpServer
                        modify (\s -> s { mcduHttpPort = Just port })
                        when serverEnabled mcduStartHttpServer
                        return True
              getPort :: MCDU (Maybe ByteString)
              getPort =
                gets (fmap (BS8.pack . show) . mcduHttpPort)

          let setHostname :: Maybe ByteString -> MCDU Bool
              setHostname hostnameMay =
                case hostnameMay of
                  Nothing ->
                    True <$ modify (\s -> s { mcduHttpHostname = Nothing })
                  Just hostname -> do
                    modify (\s -> s { mcduHttpHostname = Just (map toLower $ BS8.unpack hostname) })
                    return True
              getHostname :: MCDU (Maybe ByteString)
              getHostname =
                gets (fmap BS8.pack . mcduHttpHostname)

          port <- gets mcduHttpPort
          hostname <- gets mcduHttpHostname
          modifyView $ \v -> v
            { mcduViewDraw = do
                mcduPrintC (screenW `div` 2) 1 white "--- HTTP SERVER ---"
                mcduPrint 1 3 white "HOSTNAME"
                mcduPrint 1 4 green (maybe "----" BS8.pack hostname)
                mcduPrintR (screenW - 1) 3 white "PORT"
                mcduPrintR (screenW - 1) 4 green (maybe "----" (BS8.pack . show) port)
                if headless then do
                  if serverEnabled then
                    mcduPrint 1 2 green "FORCED ON"
                  else
                    mcduPrint 1 2 red "OFF"
                  mcduPrintR (screenW - 1) 2 white "(HEADLESS)"
                else do
                  mcduPrint 1 2 green (if serverEnabled then "ON" else "OFF")
            , mcduViewLSKBindings = Map.fromList $
                [ (4, ("MAIN MENU", loadViewByID MainMenuView))
                , (1, ("", scratchInteract setHostname getHostname >> reloadView))
                , (6, ("", scratchInteract setPort getPort >> reloadView))
                ]
                ++
                (
                  if not serverEnabled || not headless then
                    [ (0, ( ""
                          , do
                              if serverEnabled then
                                mcduStopHttpServer
                              else
                                mcduStartHttpServer
                              reloadView
                          )
                      )
                    ]
                  else
                    [ (0, ("", modify (\s -> s { mcduScratchMessage = Just "NOT ALLOWED" }) >> redrawScratch)) ]
                )
                ++
                [ (7, ("QR", mcduPrintHttpServerQR)) | serverEnabled ]
            }
        _ -> modifyView $ \v -> v
              { mcduViewDraw = do
                  mcduPrintC (screenW `div` 2) (screenH `div` 2) red "INVALID PAGE"
              , mcduViewLSKBindings = mempty
              }
  }


