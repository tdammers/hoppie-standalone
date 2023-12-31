{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Config
where

import Web.Hoppie.System
import Web.Hoppie.FGFS.FMS
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
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
import Text.Casing

configView :: MCDUView
configView = defView
  { mcduViewTitle = "CONFIG"
  , mcduViewNumPages = 3
  , mcduViewOnLoad = do
      page <- gets (mcduViewPage . mcduView)

      let setMyCallsign (Just c) = do
            mcduSetCallsign c
            return True
          setMyCallsign Nothing =
            return False
          getMyCallsign =
            Just <$> mcduGetCallsign

      case page of 
        0 -> do
          actype <- gets mcduAircraftType
          showLog <- gets mcduShowLog
          headless <- gets mcduHeadless
          callsign <- mcduGetCallsign
          units <- gets mcduMassUnit
          serverEnabled <- gets (isJust . mcduHttpServer)
          atisSrc <- lift $ getAtisSource
          let setACType :: Maybe ByteString -> MCDU Bool
              setACType actypeMay = do
                modify (\s -> s { mcduAircraftType = actypeMay })
                persistData
                return True
              getACType :: MCDU (Maybe ByteString)
              getACType =
                gets mcduAircraftType

              toggleAtisSource =
                case atisSrc of
                  Just AtisSourceVatsimDatafeed ->
                    lift $ setAtisSource AtisSourceHoppie
                  Just AtisSourceHoppie ->
                    lift $ setAtisSource AtisSourceVatsimDatafeed
                  Nothing ->
                    lift $ setAtisSource AtisSourceVatsimDatafeed

          modifyView $ \v -> v
            { mcduViewTitle = "CONFIG"
            , mcduViewAutoReload = False
            , mcduViewDraw = do
                mcduPrint 1 1 white "A/C TYPE"
                mcduPrint 1 2 green (fromMaybe "----" actype)
                mcduPrintR (screenW - 1) 1 white "CALLSIGN"
                mcduPrintR (screenW - 1) 2 green callsign

                mcduPrint 1 3 white "UNITS"
                mcduPrint 1 4 green (case units of { Kilograms -> "KG"; Pounds -> "LBS" })

                mcduPrint 1 5 white "LOG"
                mcduPrint 1 6 green (if showLog then "ON" else "OFF")
                mcduPrintR (screenW - 1) 5 white "HEADLESS"
                if serverEnabled then do
                  mcduPrintR (screenW - 1) 6 green (if headless then "ON" else "OFF")
                else do
                  if headless then
                    mcduPrintR (screenW - 1) 6 red "ON"
                  else
                    mcduPrintR (screenW - 1) 6 yellow "DISABLED"
                  mcduPrintR (screenW - 1) 7 white "(HTTP SERVER OFF)"
                mcduPrint 1 7 white "ATIS SRC"
                mcduPrint 1 8 green (maybe "----" (BS8.pack . map toUpper . toWords . dropPrefix . dropPrefix . fromHumps . show) atisSrc)
            , mcduViewLSKBindings = Map.fromList $ 
                [ (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
                , (LSKL 0, ("", scratchInteract setACType getACType >> reloadView))
                , (LSKR 0, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                , (LSKL 1, ("", do
                             modify (\s -> s
                               { mcduMassUnit = case units of
                                   Kilograms -> Pounds
                                   Pounds -> Kilograms
                               })
                             reloadView
                             persistData
                           )
                  )
                , (LSKL 2, ("", modify (\s -> s { mcduShowLog = not (mcduShowLog s) }) >> flushAll >> reloadView))
                , (LSKL 3, ("", toggleAtisSource >> reloadView))
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
            { mcduViewTitle = "HTTP CONFIG"
            , mcduViewAutoReload = False
            , mcduViewDraw = do
                mcduPrint 1 3 white "HTTP SERVER ENABLE"
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
        2 -> do
          fgConnected <- gets (isJust . mcduFlightgearConnection)
          let setPort :: Maybe ByteString -> MCDU Bool
              setPort portMay =
                case portMay of
                  Nothing ->
                    True <$ modify (\s -> s { mcduFlightgearPort = Nothing })
                  Just portStr ->
                    case readMaybe . BS8.unpack $ portStr of
                      Nothing -> return False
                      Just port -> do
                        when fgConnected mcduDisconnectFlightgear
                        modify (\s -> s { mcduFlightgearPort = Just port })
                        when fgConnected mcduConnectFlightgear
                        return True
              getPort :: MCDU (Maybe ByteString)
              getPort =
                gets (fmap (BS8.pack . show) . mcduFlightgearPort)

          let setHostname :: Maybe ByteString -> MCDU Bool
              setHostname hostnameMay =
                case hostnameMay of
                  Nothing ->
                    True <$ modify (\s -> s { mcduFlightgearHostname = Nothing })
                  Just hostname -> do
                    when fgConnected mcduDisconnectFlightgear
                    modify (\s -> s { mcduFlightgearHostname = Just (map toLower $ BS8.unpack hostname) })
                    when fgConnected mcduConnectFlightgear
                    return True
              getHostname :: MCDU (Maybe ByteString)
              getHostname =
                gets (fmap BS8.pack . mcduFlightgearHostname)

          port <- gets mcduFlightgearPort
          hostname <- gets mcduFlightgearHostname
          connect <- gets mcduFlightgearConnect
          syncCallsign <- gets mcduFlightgearSyncCallsign
          callsignLocal <- lift getCallsign
          callsignFG <- if fgConnected then
                          getFGCallsign
                        else
                          return Nothing

          modifyView $ \v -> v
            { mcduViewTitle = "FGFS CONFIG"
            , mcduViewAutoReload = True
            , mcduViewDraw = do
                mcduPrint 1 1 white "FGFS TELNET CONNECTION"
                if connect then
                  mcduPrintColored 1 2 (if fgConnected then colorize green "ON" else colorize yellow "CONNECTING")
                else
                  mcduPrintColored 1 2 (if fgConnected then colorize cyan "DISCONNECTING" else colorize white "OFF")
                mcduPrint 1 3 white "HOSTNAME"
                mcduPrintR (screenW - 1) 3 white "PORT"
                mcduPrint 1 4 green (maybe "----" BS8.pack hostname)
                mcduPrintR (screenW - 1) 4 green (maybe "----" (BS8.pack . show) port)
                mcduPrint 1 5 white "SYNC CALLSIGN"
                mcduPrint 1 6 green (if syncCallsign then "ON" else "OFF")
                mcduPrint 1 7 white "FG:"
                mcduPrint 1 8 (if syncCallsign then cyan else blue) (fromMaybe "----" callsignFG)
                mcduPrintR (screenW - 1) 7 white "MCDU:"
                mcduPrintR (screenW - 1) 8 green callsignLocal
            , mcduViewLSKBindings = Map.fromList $
                [ (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
                , (LSKL 0, ( ""
                           , do
                               if connect then do
                                 modify $ \s -> s { mcduFlightgearConnect = False }
                                 when fgConnected mcduDisconnectFlightgear
                               else do
                                 modify $ \s -> s { mcduFlightgearConnect = True }
                                 unless fgConnected mcduConnectFlightgear
                               reloadView
                           )
                  )
                , (LSKL 1, ("", scratchInteract setHostname getHostname >> reloadView))
                , (LSKR 1, ("", scratchInteract setPort getPort >> reloadView))
                , (LSKL 2, ("", do
                             modify $ \s -> s { mcduFlightgearSyncCallsign = not syncCallsign }
                             -- trigger re-sync
                             void mcduGetCallsign
                             reloadView
                           )
                  )
                , (LSKR 3, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                ]
            }
        _ -> modifyView $ \v -> v
              { mcduViewDraw = do
                  mcduPrintC (screenW `div` 2) (screenH `div` 2) red "INVALID PAGE"
              , mcduViewLSKBindings = mempty
              }
  }


