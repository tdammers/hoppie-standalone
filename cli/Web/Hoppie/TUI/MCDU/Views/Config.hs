{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Config
where

import Web.Hoppie.System
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.StringUtil

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Read (readMaybe)
import System.IO

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
                mcduPrint 0 1 white "A/C TYPE"
                mcduPrint 0 2 green (fromMaybe "----" actype)
                mcduPrintR (screenW - 1) 1 white "CALLSIGN"
                mcduPrintR (screenW - 1) 2 green callsign
                mcduPrintR (screenW - 1) 3 white "SHOW LOG"
                mcduPrintR (screenW - 1) 4 green (if showLog then "ON" else "OFF")
                mcduPrintR (screenW - 1) 5 white "HEADLESS MODE"
                if serverEnabled then do
                  mcduPrintR (screenW - 1) 6 green (if headless then "ON" else "OFF")
                else do
                  if headless then
                    mcduPrintR (screenW - 1) 6 red "ON"
                  else
                    mcduPrintR (screenW - 1) 6 yellow "DISABLED"
                  mcduPrintR (screenW - 1) 7 white "(HTTP SERVER OFF)"
            , mcduViewLSKBindings = Map.fromList $ 
                [ (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
                , (LSKL 0, ("", scratchInteract setACType getACType >> reloadView))
                , (LSKR 0, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                , (LSKR 1, ("", modify (\s -> s { mcduShowLog = not (mcduShowLog s) }) >> flushAll >> reloadView))
                ]
                ++
                if serverEnabled then
                  [ (LSKR 2, ("", do
                        modify (\s -> s
                          { mcduHeadless = not (mcduHeadless s) })
                        -- gets mcduHeadless >>= liftIO . hSetEcho stdin
                        headless' <- gets mcduHeadless
                        when headless' $ liftIO $ do
                          clearScreen
                          moveTo 0 0
                          hFlush stdout
                        debugPrint $ colorize blue $ "Headless mode " <> if headless' then "ON" else "OFF"
                        flushAll
                        reloadView
                      )
                    )
                  ]
                else
                  [ (LSKR 2, ("", modify (\s -> s { mcduScratchMessage = Just "NOT ALLOWED" }) >> redrawScratch)) ]
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
                mcduPrintC (screenW `div` 2) 2 white "HTTP SERVER"
                mcduPrint 1 3 white "ENABLE"
                mcduPrint 1 5 white "HOSTNAME"
                mcduPrint 1 6 green (maybe "----" BS8.pack hostname)
                mcduPrintR (screenW - 1) 5 white "PORT"
                mcduPrintR (screenW - 1) 6 green (maybe "----" (BS8.pack . show) port)
                if headless then do
                  if serverEnabled then
                    mcduPrint 1 4 green "FORCED ON"
                  else
                    mcduPrint 1 4 red "OFF"
                  mcduPrintR (screenW - 1) 4 white "(HEADLESS)"
                else do
                  mcduPrint 1 4 green (if serverEnabled then "ON" else "OFF")
            , mcduViewLSKBindings = Map.fromList $
                [ (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
                , (LSKL 2, ("", scratchInteract setHostname getHostname >> reloadView))
                , (LSKR 2, ("", scratchInteract setPort getPort >> reloadView))
                ]
                ++
                (
                  if not serverEnabled || not headless then
                    [ (LSKL 1, ( ""
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
                    [ (LSKL 1, ("", modify (\s -> s { mcduScratchMessage = Just "NOT ALLOWED" }) >> redrawScratch)) ]
                )
                ++
                [ (LSKR 3, ("QR", mcduPrintHttpServerQR)) | serverEnabled ]
            }
        _ -> modifyView $ \v -> v
              { mcduViewDraw = do
                  mcduPrintC (screenW `div` 2) (screenH `div` 2) red "INVALID PAGE"
              , mcduViewLSKBindings = mempty
              }
  }


