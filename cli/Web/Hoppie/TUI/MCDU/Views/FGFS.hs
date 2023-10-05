{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
where

import Web.Hoppie.FGFS.FMS as FMS
import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex
import Web.Hoppie.Trans

import Control.Exception
import Control.Monad
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Word
import Text.Printf
import Text.Read (readMaybe)

formatETE :: Double -> String
formatETE minutesRaw =
  let (hours, minutes) = floor minutesRaw `divMod` 60 :: (Int, Int)
  in
    if hours >= 24 then
      "+++++"
    else
      printf "%02i+%02i" hours minutes

formatDistance :: Double -> String
formatDistance dist
  | dist < 10
  = printf "%4.1fNM" dist
  | otherwise
  = printf "%4.0fNM" dist

formatDistanceCompact :: Double -> String
formatDistanceCompact dist
  | dist < 10
  = printf "%4.1f" dist
  | otherwise
  = printf "%4.0f" dist

formatAltitude :: Maybe Double -> Maybe Text -> String
formatAltitude (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= 18000
         -> printf "%5.0f" alt
      () | otherwise
         -> printf "FL%3.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatAltitude _ _ = "  ---"

formatSpeed :: Maybe Double -> Maybe Text -> String
formatSpeed (Just speed) (Just cstr) =
  speedStr ++ conStr
  where
    speedStr = printf "%3.0f" speed
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatSpeed _ _ = "---"

formatEFOB :: Word8 -> Double -> Double -> Maybe Double -> Colored ByteString
formatEFOB _ _ _ Nothing = ""
formatEFOB defcolor finres cont (Just efob)
  | efob <= 0
  = colorize red "---/-"
  | otherwise
  = colorize color . BS8.pack $ printf "%5.1f" (abs efob / 1000)
  where
    color
      | efob <= finres
      = red
      | efob <= finres + cont
      = yellow
      | otherwise
      = defcolor

formatAltitudeCompact :: Maybe Double -> Maybe Text -> String
formatAltitudeCompact (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= 18000
         -> printf "%1.0f" alt
      () | otherwise
         -> printf "FL%1.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> ""
      _ -> ""
formatAltitudeCompact _ _ = "-"

formatSpeedCompact :: Maybe Double -> Maybe Text -> String
formatSpeedCompact (Just speed) (Just cstr) =
  speedStr ++ conStr
  where
    speedStr = printf "%1.0f" speed
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> ""
      _ -> ""
formatSpeedCompact _ _ = "-"

formatETA :: Double -> String
formatETA eta =
  let (minutesRaw :: Int) = floor eta `mod` (24 * 60)
      (hours, minutes) = minutesRaw `divMod` 60
  in printf "%02i%02i" hours minutes

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

loadWaypointSelect :: ByteString
                   -> [WaypointCandidate]
                   -> (Maybe WaypointCandidate -> MCDU ())
                   -> MCDU ()
loadWaypointSelect returnTitle candidates cont = do
  loadView $ selectViewWith
    SelectViewOptions
      { selectViewSingleSided = True
      , selectViewBreakLines = True
      , selectViewUnloadAction = mapM_ releaseWaypointCandidate candidates
      }
    "SELECT WPT"
    [ (c, colorize color . BS8.pack $ printf "%s\n%s %s %03.0f°"
                      (Text.take (screenW - 2) (wpName c))
                      (Text.toUpper $ wpType c)
                      (formatDistance $ wpDistance c)
                      (wpBearing c)
      )
    | c <- candidates
    , let color = case wpType c of
                    "leg" -> green
                    _ -> cyan
    ]
    returnTitle
    (\candidateMay -> do
        cont candidateMay
    )

mcduResolveWaypoint :: ByteString -> ByteString -> (Maybe WaypointCandidate -> MCDU ()) -> MCDU ()
mcduResolveWaypoint returnTitle name cont =
  resolveWaypoint name scratchWarn (loadWaypointSelect returnTitle) cont

loadDirectToView :: Maybe ByteString -> MCDU ()
loadDirectToView Nothing = do
  loadDirectToViewWith Nothing Nothing
loadDirectToView (Just toName) = do
  mcduResolveWaypoint "NO WPT" toName (loadDirectToViewWith Nothing)

loadDirectToViewWith :: Maybe WaypointCandidate -> Maybe WaypointCandidate -> MCDU ()
loadDirectToViewWith fromMayOrig toMayOrig = do
  fromVar :: IORef (Maybe WaypointCandidate) <- liftIO $ newIORef Nothing
  toVar :: IORef (Maybe WaypointCandidate) <- liftIO $ newIORef Nothing

  let release :: IORef (Maybe WaypointCandidate) -> MCDU ()
      release var =
        liftIO (readIORef var) >>= mapM_ releaseWaypointCandidate

      acquire :: IORef (Maybe WaypointCandidate) -> MCDU ()
      acquire var =
        liftIO (readIORef var) >>= mapM_ acquireWaypointCandidate

      erase :: IORef (Maybe WaypointCandidate) -> MCDU ()
      erase var = do
        release var
        liftIO $ writeIORef var Nothing

      store :: IORef (Maybe WaypointCandidate) -> Maybe WaypointCandidate -> MCDU ()
      store var wpMay = do
        release var
        liftIO $ writeIORef var wpMay
        acquire var

      reload :: MCDU ()
      reload = do
        acquire toVar
        acquire fromVar
        loadView directToView

      setTo Nothing = do
        erase toVar
        reload
        return True
      setTo (Just n) = do
        mcduResolveWaypoint "DIRECT TO" n $ \wpMay -> do
          store toVar wpMay
          reload
        return True
      getTo =
        liftIO $ fmap (encodeUtf8 . wpID) <$> readIORef toVar

      setFrom Nothing = do
        erase fromVar
        reload
        return True
      setFrom (Just n) = do
        resolveLeg n $ \wpMay -> do
          store fromVar wpMay
          reload
        return True
      getFrom =
        liftIO $ fmap (encodeUtf8 . wpID) <$> readIORef fromVar

      insert = do
        fromMay <- liftIO $ readIORef fromVar
        toMay <- liftIO $ readIORef toVar

        case toMay of
          Nothing -> do
            scratchWarn "INVALID"
            reload
          Just toWP -> do
            done <- insertDirect fromMay toWP
            case done of
              Left err -> do
                scratchWarn (encodeUtf8 err)
                reload
              Right () ->
                loadView fplView

      directToView = defView
        { mcduViewTitle = "DIRECT TO"
        , mcduViewOnLoad = do
            to <- liftIO $ readIORef toVar
            from <- liftIO $ readIORef fromVar
            let wpColor w = case wpType <$> w of
                  Nothing -> blue
                  Just "leg" -> green
                  Just _ -> cyan
                wpLabel = maybe "----" (\wp ->
                    encodeUtf8 $ wpID wp <> " (" <> Text.toUpper (wpType wp) <> ")"
                  )
                wpDesc = maybe "" (encodeUtf8 . wpName)
                wpInfo = maybe ""
                          (\wp -> BS8.pack $ printf "%s %03.0f°"
                            (formatDistance $ wpDistance wp)
                            (wpBearing wp)
                          )
            -- from <- liftIO $ readIORef fromVar
            modifyView $ \v -> v
              { mcduViewDraw = do
                  mcduPrint 1 1 white "TO"
                  mcduPrint 1 2 (wpColor to) (wpLabel to)
                  mcduPrint 1 3 (wpColor to) (wpDesc to)
                  mcduPrint 1 4 (wpColor to) (wpInfo to)
                  mcduPrint 1 5 white "FROM"
                  mcduPrint 1 6 (wpColor from) (wpLabel from)
                  mcduPrint 1 7 (wpColor from) (wpDesc from)
                  mcduPrint 1 8 (wpColor from) (wpInfo from)
              , mcduViewLSKBindings = Map.fromList
                  [ (LSKL 0, ("", scratchInteract setTo getTo))
                  , (LSKL 2, ("", scratchInteract setFrom getFrom))
                  , (LSKR 5, ("INSERT", insert))
                  ]
              }
        , mcduViewOnUnload = do
            release fromVar
            release toVar
        }
  store toVar toMayOrig
  store fromVar fromMayOrig
  loadView directToView

parseRestrictions :: ByteString -> Either ByteString (Maybe Int, ByteString, Maybe Int, ByteString)
parseRestrictions rbs = runExcept $ do
  let rstr = decodeUtf8 rbs
  (spdStr, altStr) <- case Text.splitOn "/" rstr of
    [lhs, rhs] -> return (Text.strip lhs, Text.strip rhs)
    _ -> throwError "INVALID"
  (speedMay, speedType) <- parseSpeedRestriction spdStr
  (altMay, altType) <- parseAltitudeRestriction altStr
  return (speedMay, speedType, altMay, altType)

parseSpeedRestriction :: Text -> Except ByteString (Maybe Int, ByteString)
parseSpeedRestriction str
  | Text.null str
  = return (Nothing, "keep")
  | Text.all (== '-') str
  = return (Nothing, "at")
  | otherwise
  = do
      let (valStr, restrictionStr) = Text.span isDigit str
      val <- maybe (throwError "INVALID") return $ readMaybe (Text.unpack valStr)
      restriction <- case restrictionStr of
        "" -> return "at"
        "A" -> return "above"
        "B" -> return "below"
        _ -> throwError "INVALID"
      return (Just val, restriction)

parseAltitudeRestriction :: Text -> Except ByteString (Maybe Int, ByteString)
parseAltitudeRestriction str
  | Text.null str
  = return (Nothing, "keep")
  | Text.all (== '-') str
  = return (Nothing, "at")
  | "FL" `Text.isPrefixOf` str
  = do
      (valMay, restriction) <- parseAltitudeRestriction (Text.drop 2 str)
      return ((* 100) <$> valMay, restriction)
  | otherwise
  = do
      let (valStr, restrictionStr) = Text.span isDigit str
      val <- maybe (throwError "INVALID") return $ readMaybe (Text.unpack valStr)
      restriction <- case restrictionStr of
        "" -> return "at"
        "A" -> return "above"
        "B" -> return "below"
        _ -> throwError "INVALID"
      return (Just val, restriction)

perfInitView :: MCDUView
perfInitView = defView
  { mcduViewTitle = "PERF INIT"
  , mcduViewAutoReload = False
  , mcduViewOnLoad = perfInitViewLoad
  , mcduViewNumPages = 2
  }

perfInitViewLoad :: MCDU ()
perfInitViewLoad = withFGView $ do
  pdVar <- liftIO . newIORef =<< getPerfInitData

  let doLoad = do
        massUnit <- gets mcduMassUnit
        let massFactor = case massUnit of
                            Kilograms -> 1
                            Pounds -> 1 / lbs2kg
            massUnitStr = case massUnit of
                            Kilograms -> "KG"
                            Pounds -> "LBS"
        let setMyCallsign (Just c) = do
              mcduSetCallsign c
              return True
            setMyCallsign Nothing =
              return False
            getMyCallsign =
              Just <$> mcduGetCallsign

        pd <- liftIO $ readIORef pdVar
        pdStored <- getPerfInitData
        currentFuel <- getFuelOnBoard

        let setZFW Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitZFW = Nothing }
              return True
            setZFW (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitZFW = Just (x / massFactor) }
                  return True
            getZFW =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitZFW pd

        let setBlockFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitBlockFuel = Nothing }
              return True
            setBlockFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitBlockFuel = Just (x / massFactor) }
                  return True
            getBlockFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitBlockFuel pd

        let setMinTakeoffFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitMinTakeoffFuel = Nothing }
              return True
            setMinTakeoffFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitMinTakeoffFuel = Just (x / massFactor) }
                  return True
            getMinTakeoffFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitMinTakeoffFuel pd

        let setContingencyFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitContingencyFuel = Nothing }
              return True
            setContingencyFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitContingencyFuel = Just (x / massFactor) }
                  return True
            getContingencyFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitContingencyFuel pd

        let setReserveFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitReserveFuel = Nothing }
              return True
            setReserveFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitReserveFuel = Just (x / massFactor) }
                  return True
            getReserveFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitReserveFuel pd

        let confirmInit = do
              setPerfInitData pd
              reloadView

            resetData = do
              getPerfInitData >>= liftIO . writeIORef pdVar
              reloadView

            isModified getter = do
              ((floor . (* 10) <$> getter pd) :: Maybe Int) /= (floor . (* 10) <$> getter pdStored)

            fieldColor getter =
              if isModified getter then
                cyan
              else
                green

            modificationsExist =
              any isModified
                [ perfInitZFW
                , perfInitBlockFuel
                , perfInitMinTakeoffFuel
                , perfInitContingencyFuel
                , perfInitReserveFuel
                ]

        actype <- gets mcduAircraftType
        callsign <- mcduGetCallsign
        let setACType :: Maybe ByteString -> MCDU Bool
            setACType actypeMay = do
              modify (\s -> s { mcduAircraftType = actypeMay })
              persistData
              return True
            getACType :: MCDU (Maybe ByteString)
            getACType =
              gets mcduAircraftType

        curPage <- gets (mcduViewPage . mcduView)
        case curPage of
          0 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT"
              , mcduViewDraw = do
                  mcduPrint 1 1 white "A/C TYPE"
                  mcduPrint 1 2 green (fromMaybe "----" actype)
                  mcduPrintR (screenW - 1) 1 white "ATC C/S"
                  mcduPrintR (screenW - 1) 2 green callsign

                  mcduPrint 1 5 white "UNITS"
                  mcduPrint 1 6 green (case massUnit of { Kilograms -> "KG"; Pounds -> "LBS" })
              , mcduViewLSKBindings = Map.fromList $
                  [ (LSKL 0, ("", scratchInteract setACType getACType >> reloadView))
                  , (LSKR 0, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                  , (LSKL 2, ("", do
                               modify (\s -> s
                                 { mcduMassUnit = case massUnit of
                                     Kilograms -> Pounds
                                     Pounds -> Kilograms
                                 })
                               reloadView
                               persistData
                             )
                    )
                  ]
              }
          1 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT " <> massUnitStr
              , mcduViewDraw = do
                  let formatMass :: Maybe Double -> ByteString
                      formatMass = maybe "-----" (BS8.pack . printf "%7.0f" . (* massFactor))
                  mcduPrintR (screenW - 1) 1 white "ZFW"
                  mcduPrintR (screenW - 1) 2 (fieldColor perfInitZFW) $ formatMass (perfInitZFW pd)
                  mcduPrint 1 3 white "GAUGE"
                  mcduPrint 1 4 white $ formatMass currentFuel
                  mcduPrintR (screenW - 1) 3 white "BLOCK FUEL"
                  mcduPrintR (screenW - 1) 4 (fieldColor perfInitBlockFuel) $ formatMass (perfInitBlockFuel pd)
                  mcduPrintR (screenW - 1) 5 white "T/O FUEL"
                  mcduPrintR (screenW - 1) 6 (fieldColor perfInitMinTakeoffFuel) $ formatMass (perfInitMinTakeoffFuel pd)
                  mcduPrintR (screenW - 1) 7 white "CONT FUEL"
                  mcduPrintR (screenW - 1) 8 (fieldColor perfInitContingencyFuel) $ formatMass (perfInitContingencyFuel pd)
                  mcduPrintR (screenW - 1) 9 white "RSRV FUEL"
                  mcduPrintR (screenW - 1) 10 (fieldColor perfInitReserveFuel) $ formatMass (perfInitReserveFuel pd)
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKR 0, ("", scratchInteract setZFW getZFW >> reloadView))
                  , ( LSKR 1, ("", scratchInteract setBlockFuel getBlockFuel >> reloadView))
                  , ( LSKR 2, ("", scratchInteract setMinTakeoffFuel getMinTakeoffFuel >> reloadView))
                  , ( LSKR 3, ("", scratchInteract setContingencyFuel getContingencyFuel >> reloadView))
                  , ( LSKR 4, ("", scratchInteract setReserveFuel getReserveFuel >> reloadView))
                  ] ++
                  [ ( LSKL 5, ("RESET", resetData)) | modificationsExist ] ++
                  [ ( LSKR 5, ("CONFIRM INIT", confirmInit)) | modificationsExist ]
              }
          _ -> modifyView $ \v -> v
                { mcduViewDraw = do
                    mcduPrintC (screenW `div` 2) (screenH `div` 2) red "INVALID PAGE"
                , mcduViewLSKBindings = mempty
                }

  modifyView $ \v -> v
    { mcduViewOnLoad = doLoad
    , mcduViewAutoReload = True
    }
  doLoad

progView :: MCDUView
progView = defView
  { mcduViewTitle = "PROGRESS"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = progViewLoad
  }

progViewLoad :: MCDU ()
progViewLoad = withFGView $ do
  progress <- getProgressInfo
  massUnit <- gets mcduMassUnit
  let massFactor = case massUnit of
                      Kilograms -> 1
                      Pounds -> 1 / lbs2kg
      massUnitStr = case massUnit of
                      Kilograms -> "KG"
                      Pounds -> "LBS"
  perfInit <- getPerfInitData
  let finres = maybe 0 (* massFactor) $ perfInitReserveFuel perfInit
      cont = maybe 0 (* massFactor) $ perfInitContingencyFuel perfInit
  modifyView $ \v -> v
    { mcduViewDraw = do
        let printWP color y (Just wp) = do
              mcduPrint 0 y color (encodeUtf8 . Text.take 7 . Text.replace "-" "" $ legName wp)
              forM_ (legETE wp) $ \ete ->
                mcduPrint 13 y color (BS8.pack $ formatETE ete)
              forM_ (legRemainingDist wp) $ \dist ->
                mcduPrintR 12 y color (BS8.pack $ formatDistanceCompact dist)
              mcduPrintColoredR 24 y (formatEFOB color finres cont ((* massFactor) <$> legEFOB wp))
            printWP color y Nothing = do
              mcduPrint 0 y color "-----"
              mcduPrintColoredR 24 y (formatEFOB color finres cont Nothing)
        mcduPrint 1 1 white "TO"
        mcduPrint 8 1 white "DIST"
        mcduPrint 14 1 white "ETE"
        mcduPrint 20 1 white "FUEL"
        printWP magenta 2 (progressCurrent =<< progress)

        mcduPrint 1 3 white "NEXT"
        printWP green 4 (progressNext =<< progress)

        mcduPrint 1 5 white "DEST"
        printWP green 6 (progressDestination =<< progress)

        mcduPrintR (screenW - 1) 7 white "FOB"
        mcduPrintColoredR (screenW - 1) 8 $
          ( colorize green $
            maybe
              "-----"
              (\fob -> 
                let fobU = floor (fob * massFactor) :: Int
                in BS8.pack $ printf "%5i" fobU
              )
              (progressFOB =<< progress)
          ) <>
          " " <>
          ( colorize white massUnitStr )

    }

fplView :: MCDUView
fplView = defView
  { mcduViewTitle = "ACT FPL"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = fplViewLoad
  }

fplViewLoad :: MCDU ()
fplViewLoad = withFGView $ do
  let legsPerPage = numLSKs - 1
  curPage <- gets (mcduViewPage . mcduView)

  (groundspeed :: Double) <- max 100 <$> getGroundspeed
  (utcMinutes :: Double) <- getUTCMinutes

  planSize <- getFlightplanSize
  currentLeg <- fmap (max 1) <$> getCurrentLeg

  curLegs <- getFlightplanLegs legsPerPage curPage (fromMaybe 1 currentLeg - 1)
  flightplanModified <- hasFlightplanModifications
  let legsDropped = curPage * legsPerPage
  let numPages = (planSize - fromMaybe 0 currentLeg + legsPerPage) `div` legsPerPage
  massUnit <- gets mcduMassUnit
  let massFactor = case massUnit of
                      Kilograms -> 1
                      Pounds -> 1 / lbs2kg
  perfInit <- getPerfInitData
  liftIO $ print perfInit
  let finres = maybe 0 (* massFactor) $ perfInitReserveFuel perfInit
      cont = maybe 0 (* massFactor) $ perfInitContingencyFuel perfInit

  let putWaypoint :: Int -> Maybe ByteString -> MCDU Bool
      putWaypoint n Nothing = deleteWaypoint n
      putWaypoint n (Just targetWPName) = do
        mcduResolveWaypoint "FPL" targetWPName $ \case
          Nothing -> do
            loadView fplView
          Just toWP -> do
            getFlightplanLeg n $ \fromWPMay -> do
              toIndexMay <- findFPWaypoint toWP
              fromIndexMay <- join <$> mapM findFPWaypoint fromWPMay
              case (toIndexMay :: Maybe Int, fromIndexMay :: Maybe Int) of
                (Nothing, Nothing) -> do
                  scratchWarn "INVALID"
                  releaseWaypointCandidate toWP
                  mapM_ releaseWaypointCandidate fromWPMay
                  reloadView
                (Just toIndex, Just fromIndex) ->
                  if toIndex < fromIndex then
                    loadDirectToViewWith (Just toWP) fromWPMay
                  else
                    loadDirectToViewWith fromWPMay (Just toWP)
                _ ->
                  loadDirectToViewWith fromWPMay (Just toWP)
        return True
      getWaypoint n = do
        getWaypointName n

      putRestrictions :: Int -> Maybe ByteString -> MCDU Bool
      putRestrictions n Nothing = do
        err1 <- setFPLegSpeed n Nothing ""
        case err1 of
          Just e -> do
            scratchWarn e
            return False
          Nothing -> do
            err2 <- setFPLegAltitude n Nothing ""
            case err2 of
              Just e -> do
                scratchWarn e
                return False
              Nothing ->
                return True
      putRestrictions n (Just rbs) = do
        let parseResult = parseRestrictions rbs
        case parseResult of
          Left e -> do
            scratchWarn e
            return False
          Right (speedMay, speedType, altMay, altType) -> do
            err1 <- if speedType == "keep" then
                      return Nothing
                    else
                      setFPLegSpeed n speedMay speedType
            case err1 of
              Just e -> do
                scratchWarn e
                return False
              Nothing -> do
                err2 <- if altType == "keep" then
                          return Nothing
                        else
                          setFPLegAltitude n altMay altType
                case err2 of
                  Just e -> do
                    scratchWarn e
                    return False
                  Nothing ->
                    return True
      getRestrictions n = do
        let n' = n - legsDropped + 1 - fromMaybe 0 currentLeg
        case drop n' curLegs of
          (leg:_) -> do
            let spdStr = BS8.pack $ formatSpeedCompact (legSpeed leg) (legSpeedType leg)
                altStr = BS8.pack $ formatAltitudeCompact (legAlt leg) (legAltType leg)
            return . Just $ spdStr <> "/" <> altStr
          _ -> do
            scratchWarn "NO WPT"
            return Nothing


  modifyView $ \v -> v
    { mcduViewNumPages = numPages
    , mcduViewTitle = if flightplanModified then "MOD FPL" else "ACT FPL"
    , mcduViewLSKBindings = Map.fromList $
        [ (LSKL n, ("", do
              scratchInteract
                (putWaypoint (n + legsDropped - 1 + fromMaybe 0 currentLeg))
                (getWaypoint (n + legsDropped - 1 + fromMaybe 0 currentLeg))
              reloadView
          ))
        | n <- [0 .. legsPerPage ]
        ]
        ++
        [ (LSKR n, ("", do
              scratchInteract
                (putRestrictions (n + legsDropped - 1 + fromMaybe 0 currentLeg))
                (getRestrictions (n + legsDropped - 1 + fromMaybe 0 currentLeg))
              reloadView
          ))
        | n <- [0 .. legsPerPage ]
        ]
        ++
        [ (LSKL 5, ("CANCEL", cancelFlightplanEdits >> reloadView)) | flightplanModified ]
        ++
        [ (LSKR 5, ("CONFIRM", commitFlightplanEdits >> reloadView)) | flightplanModified ]
    , mcduViewDraw = do
        when (planSize == 0) $ do
          mcduPrintC (screenW `div` 2) (screenH `div` 2) white "NO FPL"
        zipWithM_
          (\n leg -> do
            let isCurrent = n + legsDropped == 1
                isPrevious = n + legsDropped == 0
                color
                  | isPrevious = yellow
                  | isCurrent = magenta
                  | legIsDiscontinuity leg = white
                  | legRole leg == Just "missed" = cyan
                  | otherwise = green
            unless (legIsDiscontinuity leg) $ do
              forM_ (legRemainingDist leg) $ \dist -> do
                when (groundspeed > 40) $ do
                  let eta = utcMinutes + dist / groundspeed * 60
                  mcduPrint (screenW - 5) (n * 2 + 1) color (BS8.pack $ formatETA eta <> "z")
            if isPrevious then
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            else if legIsDiscontinuity leg then do
              mcduPrint 6 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legDist leg)
              mcduPrint 0 (n * 2 + 2) color "---- DISCONTINUITY ----"
            else do
              mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
              mcduPrint 6 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ if isCurrent then legRemainingDist leg else legDist leg)
              mcduPrintColored 13 (n * 2 + 1) (formatEFOB color finres cont . fmap (* massFactor) $ legEFOB leg)
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            unless (legIsDiscontinuity leg) $ do
              mcduPrint (screenW - 11) (n * 2 + 2) color (BS8.pack $ formatSpeed (legSpeed leg) (legSpeedType leg))
              mcduPrint (screenW - 7) (n * 2 + 2) color "/"
              mcduPrint (screenW - 6) (n * 2 + 2) color (BS8.pack $ formatAltitude (legAlt leg) (legAltType leg))

          ) [0,1..] curLegs
    }

rteView :: MCDUView
rteView = defView
  { mcduViewTitle = "ACT RTE"
  , mcduViewOnLoad = rteViewLoad
  }


rteViewLoad :: MCDU ()
rteViewLoad = withFGView $ do
  departureMay <- getDeparture
  destinationMay <- getDestination
  callsign <- lift getCallsign
  flightplanModified <- hasFlightplanModifications
  routeLegs <- getRoute
  curPage <- gets (mcduViewPage . mcduView)
  let (numRoutePages, curRouteLegs) = paginateWithHeadroom 2 6 (curPage - 1) routeLegs
      numPages = numRoutePages + 1
      numLegs = length routeLegs

  let setViaTo :: Maybe ByteString -> MCDU Bool
      setViaTo Nothing =
        return False
      setViaTo (Just viaToStr) = do
        let (viaStr, r) = BS.span (/= ord8 '.') viaToStr
            toStr = BS.drop 1 r
        let goResult result = do
              case result of
                Just err -> do
                  scratchWarn err
                Nothing -> do
                  return ()
              loadView rteView
              modifyView $ \v -> v
                { mcduViewPage = numLegs `div` 6 + 1 }
              reloadView
              return ()
        if BS.null toStr then do
          mcduResolveWaypoint "RTE" viaStr $ \case
            Nothing -> do
              scratchWarn "NO WPT"
              return ()
            Just wp -> do
              result <- appendDirectTo wp
              releaseWaypointCandidate wp
              goResult result
        else do
          mcduResolveWaypoint "RTE" toStr $ \case
            Nothing -> do
              scratchWarn "NO WPT"
              return ()
            Just wp -> do
              result <- appendViaTo viaStr wp
              releaseWaypointCandidate wp
              goResult result
        return True
      getViaTo :: MCDU (Maybe ByteString)
      getViaTo = return Nothing

      setLeg :: Int -> Maybe ByteString -> MCDU Bool
      setLeg i Nothing = do
        case drop i routeLegs of
          [] -> do
            scratchWarn "INVALID"
            return False
          (leg:_) -> do
            deleteRouteLeg (routeLegFromIndex leg) (routeLegToIndex leg)
            return True
      setLeg _ _ = do
        scratchWarn "NOT ALLOWED"
        return False
      getLeg :: Int -> MCDU (Maybe ByteString)
      getLeg i =
        case drop i routeLegs of
          [] -> do
            scratchWarn "INVALID"
            return Nothing
          (leg:_) -> case routeLegVia leg of
            Nothing -> return . Just $ routeLegTo leg
            Just via -> return . Just $ via <> "." <> routeLegTo leg

  modifyView $ \v -> v
    { mcduViewNumPages = numPages
    , mcduViewTitle = if flightplanModified then "MOD RTE" else "ACT RTE"
    }

  case curPage of
    0 -> do
      modifyView $ \v -> v
        { mcduViewLSKBindings = Map.fromList $
            [ (LSKL 0, ("", do
                        scratchInteract
                          setDeparture
                          getDeparture
                        reloadView))
            , (LSKL 2, ("CLEAR", clearFlightplan >> reloadView))
            ] ++
            [ (LSKL 4, ("DEPARTURE", loadView departureView))
            | isJust departureMay
            ] ++
            [ (LSKR 0, ("", do
                        scratchInteract
                          setDestination
                          getDestination
                        reloadView))
            , (LSKR 1, ("", do
                        scratchInteract
                          (maybe (return False) (\c -> mcduSetCallsign c >> return True))
                          (Just <$> lift getCallsign)
                        reloadView))
            ] ++
            [ (LSKR 4, ("ARRIVAL", loadView arrivalView))
            | isJust destinationMay
            ]
            ++
            [ (LSKL 5, ("CANCEL", cancelFlightplanEdits >> reloadView)) | flightplanModified ]
            ++
            [ (LSKR 5, ("CONFIRM", commitFlightplanEdits >> reloadView)) | flightplanModified ]
        , mcduViewDraw = do
            mcduPrint 1 1 white "ORIGIN"
            mcduPrint 1 2 green (fromMaybe "----" departureMay)
            mcduPrintR (screenW - 1) 1 white "DEST"
            mcduPrintR (screenW - 1) 2 green (fromMaybe "----" destinationMay)
            mcduPrint 1 3 white "CO RTE"
            mcduPrint 1 4 green "----------"
            mcduPrintR (screenW - 1) 3 white "FLT NO"
            mcduPrintR (screenW - 1) 4 green callsign
        }
    _ -> do
      let offset = 6 * (curPage - 1)
          newLegN = numLegs - offset
      modifyView $ \v -> v
        { mcduViewLSKBindings = mempty
        , mcduViewDraw = do
            mcduPrint  1 1 white "VIA"
            mcduPrint 10 1 white "TO"
            mcduPrintR (screenW - 1) 1 white "DIST"
            zipWithM_ (\n leg -> do
                if routeLegTo leg == "DISCONTINUITY" then do
                  mcduPrint  1 (n * 2 + 2) white "-DISCONTINUITY-"
                  mcduPrintR (screenW - 1) (n * 2 + 2) white (BS8.pack $ maybe "" formatDistance $ routeLegDistance leg)
                else do
                  mcduPrint  1 (n * 2 + 2) green (fromMaybe "DCT" $ routeLegVia leg)
                  mcduPrint 10 (n * 2 + 2) green (routeLegTo leg)
                  mcduPrintR (screenW - 1) (n * 2 + 2) green (BS8.pack $ maybe "" formatDistance $ routeLegDistance leg)
                  -- mcduPrint 1 (n * 2 + 3) blue (BS8.pack $ printf "FROM % i" (routeLegFromIndex leg))
                  -- mcduPrint 10 (n * 2 + 3) blue (BS8.pack $ printf "TO % i" (routeLegToIndex leg))
              ) [0,1..] curRouteLegs
            when (newLegN >= 0 && newLegN < 6) $ do
              mcduPrint  1 (newLegN * 2 + 2) green "-----"
              mcduPrint 10 (newLegN * 2 + 2) green "-----"
              mcduPrintR (screenW - 1) (newLegN * 2 + 2) green "-----"
        }
      zipWithM_ (\n _ -> do
          addLskBinding (LSKR n) "" (scratchInteract (setLeg (n + offset)) (getLeg (n + offset)) >> reloadView)
        ) [0,1..] curRouteLegs
      when (newLegN >= 0 && newLegN < 6) $ do
        addLskBinding
          (LSKR newLegN)
          ""
          (scratchInteract setViaTo getViaTo >> reloadView)
      when (flightplanModified && newLegN + 1 >= 0 && newLegN + 1 < 6) $ do
        addLskBinding (LSKL 5) "CANCEL" (cancelFlightplanEdits >> reloadView)
        addLskBinding (LSKR 5) "CONFIRM" (commitFlightplanEdits >> reloadView)

selectView :: ByteString -> [ByteString] -> ByteString -> (Maybe ByteString -> MCDU ()) -> MCDUView
selectView title options returnLabel handleResult= defView
  { mcduViewTitle = title
  , mcduViewNumPages = (length options + (2 * numLSKs) - 1) `div` (2 * numLSKs)
  , mcduViewLSKBindings = mempty
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      let curOptions = take (2 * numLSKs) . drop (curPage * 2 * numLSKs) $ options
      modifyView $ \v -> v {
        mcduViewLSKBindings = Map.fromList
          [ (n, (colorize white option, handleResult (Just option)))
          | (n, option) <- zip [LSKL 0 .. LSKR 4] curOptions
          ]
      }
      addLskBinding (LSKL 5) (colorize white returnLabel) (handleResult Nothing)
  }

data SelectViewOptions =
  SelectViewOptions
    { selectViewSingleSided :: Bool
    , selectViewBreakLines :: Bool
    , selectViewUnloadAction :: MCDU ()
    }


selectViewWith :: SelectViewOptions
               -> ByteString
               -> [(a, Colored ByteString)]
               -> ByteString
               -> (Maybe a -> MCDU ())
               -> MCDUView
selectViewWith svo title options returnLabel handleResult = defView
  { mcduViewTitle = title
  , mcduViewNumPages =
      (length options + itemsPerPage - 1) `div` itemsPerPage
  , mcduViewLSKBindings = mempty
  , mcduViewOnUnload = selectViewUnloadAction svo
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      let curOptions = take itemsPerPage . drop (curPage * itemsPerPage) $ options
      if (selectViewBreakLines svo) then
        modifyView $ \v -> v
          { mcduViewDraw = do
              zipWithM_ (\n (_, optionLabel) -> do
                    let y = n * 2 + 2
                        lns = take 2 $ (lineWrap (screenW - 2) optionLabel) ++ repeat ""
                    case lns of
                      [ln1, ln2] -> do
                        mcduPrint 0 y white "<"
                        mcduPrintColored 1 y ln1
                        mcduPrintColored 1 (y + 1) ln2
                      _ -> do
                        mcduPrint 0 y yellow "< WEIRDNESS"
                  ) [0..4] curOptions
          , mcduViewLSKBindings = Map.fromList
            [ (n, ("", handleResult (Just optionValue)))
            | (n, (optionValue, _)) <- zip lsks curOptions
            ]
          }
      else
        modifyView $ \v -> v {
          mcduViewLSKBindings = Map.fromList
            [ (n, (optionLabel, handleResult (Just optionValue)))
            | (n, (optionValue, optionLabel)) <- zip lsks curOptions
            ]
        }
      addLskBinding (LSKL 5) (colorize white returnLabel) (handleResult Nothing)
  }
  where
    itemsPerPage = case selectViewSingleSided svo of
      True -> numLSKs
      False -> 2 * numLSKs
    lsks = case selectViewSingleSided svo of
      True -> map LSKL [0 .. 4]
      False -> [LSKL 0 .. LSKR 4]

departureView :: MCDUView
departureView = defView
  { mcduViewTitle = "DEPARTURE"
  , mcduViewOnLoad = departureViewLoad
  }

departureViewLoad :: MCDU ()
departureViewLoad = do
  departureMay <- getDeparture

  case departureMay of
    Nothing ->
      fgErrorView "NO DEPARTURE"
    Just departure -> do
      runway <- getDepartureRunway
      sid <- getSID
      transition <- getSidTransition
      sidValid <- isValidSID
      modifyView $ \v -> v
        { mcduViewNumPages = 1
        , mcduViewLSKBindings = Map.fromList
            [ (LSKL 0, ("", do
                        scratchInteractOrSelect
                          selectDepartureRunway
                          setDepartureRunway
                        reloadView))
            , (LSKL 1, ("", do
                        scratchInteractOrSelect
                          selectSID
                          (setSID >=> warnOrSucceed)
                        reloadView))
            , (LSKL 2, ("", do
                        scratchInteractOrSelect
                          selectSidTransition
                          (setSidTransition >=> warnOrSucceed)
                        reloadView))
            , (LSKL 5, ("RTE", loadView rteView))
            ]
        , mcduViewTitle = departure <> " DEPARTURE"
        , mcduViewDraw = do
            mcduPrint 1 1 white "RUNWAY"
            mcduPrint 1 2 green (fromMaybe "----" runway)
            mcduPrint 1 3 white "SID"
            mcduPrint 1 4 (if sidValid then green else yellow) (fromMaybe "------" sid)
            mcduPrint 1 5 white "TRANSITION"
            mcduPrint 1 6 green (fromMaybe "------" transition)
        }

arrivalView :: MCDUView
arrivalView = defView
  { mcduViewTitle = "ARRIVAL"
  , mcduViewOnLoad = arrivalViewLoad
  }

arrivalViewLoad :: MCDU ()
arrivalViewLoad = do
  destinationMay <- getDestination

  case destinationMay of
    Nothing ->
      fgErrorView "NO ARRIVAL"
    Just destination -> do
      runway <- getDestinationRunway
      star <- getSTAR
      approachTransition <- getApproachTransition
      approach <- getApproach
      transition <- getStarTransition
      starValid <- isValidSTAR
      modifyView $ \v -> v
        { mcduViewNumPages = 1
        , mcduViewLSKBindings = Map.fromList
            [ (LSKL 0, ("", do
                        scratchInteractOrSelect
                          selectDestinationRunway
                          setDestinationRunway
                        reloadView))
            , (LSKL 1, ("", do
                        scratchInteractOrSelect
                          selectApproach
                          (setApproach >=> warnOrSucceed)
                        reloadView))
            , (LSKL 2, ("", do
                        scratchInteractOrSelect
                          selectSTAR
                          (setSTAR >=> warnOrSucceed)
                        reloadView))
            , (LSKR 1, ("", do
                        scratchInteractOrSelect
                          selectApproachTransition
                          (setApproachTransition >=> warnOrSucceed)
                        reloadView))
            , (LSKR 2, ("", do
                        scratchInteractOrSelect
                          selectStarTransition
                          (setStarTransition >=> warnOrSucceed)
                        reloadView))
            , (LSKL 5, ("RTE", loadView rteView))
            ]
        , mcduViewTitle = destination <> " ARRIVAL"
        , mcduViewDraw = do
            mcduPrint 1 1 white "RUNWAY"
            mcduPrint 1 2 green (fromMaybe "----" runway)
            mcduPrint 1 3 white "APPROACH"
            mcduPrint 1 4 green (fromMaybe "------" approach)
            mcduPrintR (screenW - 1) 3 white "APPR TRANS"
            mcduPrintR (screenW - 1) 4 green (fromMaybe "------" approachTransition)
            mcduPrint 1 5 white "STAR"
            mcduPrint 1 6 (if starValid then green else yellow) (fromMaybe "------" star)
            mcduPrintR (screenW - 1) 5 white "TRANSITION"
            mcduPrintR (screenW - 1) 6 green (fromMaybe "------" transition)
        }


fgErrorView :: ByteString -> MCDU ()
fgErrorView err = do
  scratchWarn err
  modifyView $ \v -> v
    { mcduViewNumPages = 1
    , mcduViewPage = 0
    , mcduViewDraw = do
        mcduPrintC (screenW `div` 2) (screenH `div` 2) red "NOT AVAIL"
    , mcduViewAutoReload = False
    , mcduViewLSKBindings = mempty
    }

withFGView :: MCDU () -> MCDU ()
withFGView go = do
  connMay <- gets mcduFlightgearConnection
  case connMay of
    Nothing -> fgErrorView "NO CONNECTION"
    Just _ -> do
      go `mcduCatches` handlers
  where
    handlers :: [MCDUHandler ()]
    handlers =
      [ MCDUHandler $ \case
          NasalUnexpected expected found -> do
            debugPrint $
              colorize red . Text.pack $
              "Nasal value error: expected " <> expected <> ", but found " <> found
            fgErrorView "SERVER ERROR"
          NasalMissingKey key -> do
            debugPrint $
              colorize red . Text.pack $
              "Nasal value error: map key " <> key <> "missing"
            fgErrorView "SERVER ERROR"
      , MCDUHandler $ \case
          NasalRuntimeError msg stackTrace -> do
            debugPrint $
              colorize red . Text.pack $
                "Nasal runtime error:" <> msg <> "\n" <>
                unlines
                  [ fromMaybe "?" fileMay <> ":" <> maybe "-" show lineMay
                  | (fileMay, lineMay) <- stackTrace
                  ]
            fgErrorView "SERVER ERROR"
      , MCDUHandler $ \(e :: SomeException) -> do
            debugPrint $
              colorize red . Text.pack $
                "Error:\n" <> show e
            fgErrorView "ERROR"
      ]

selectWith :: MCDU [ByteString]
           -> ByteString
           -> ByteString
           -> (ByteString -> MCDU Bool)
           -> ByteString
           -> MCDUView
           -> MCDU ()
selectWith getItems selectTitle warnMsg handleValue returnTitle returnView = do
  itemsMay <- getItems
  case itemsMay of
    [] -> do
      scratchWarn warnMsg
    items -> do
      let handleResult Nothing = loadView returnView
          handleResult (Just item) = do
            handleValue item >>= \case
              True ->
                loadView returnView
              False -> do
                return ()
      loadView (selectView ("SELECT " <> selectTitle) items returnTitle handleResult)

warnOrSucceed :: Maybe ByteString -> MCDU Bool
warnOrSucceed Nothing = return True
warnOrSucceed (Just e) = do
  scratchWarn e
  return False

selectDepartureRunway :: MCDU ()
selectDepartureRunway =
  selectWith
    listDepartureRunways
    "RUNWAY"
    "NO RUNWAYS"
    (setDepartureRunway . Just)
    "DEPARTURE"
    departureView

selectSID :: MCDU ()
selectSID =
  selectWith
    listSIDs
    "SID"
    "NO SIDS"
    ((setSID >=> warnOrSucceed) . Just)
    "DEPARTURE"
    departureView


selectSidTransition :: MCDU ()
selectSidTransition =
  selectWith
    listSidTransitions
    "TRANSITION"
    "NO TRANSITIONS"
    ((setSidTransition >=> warnOrSucceed) . Just)
    "DEPARTURE"
    departureView


selectDestinationRunway :: MCDU ()
selectDestinationRunway =
  selectWith
    listDestinationRunways
    "RUNWAY"
    "NO RUNWAYS"
    (setDestinationRunway . Just)
    "ARRIVAL"
    arrivalView

selectSTAR :: MCDU ()
selectSTAR =
  selectWith
    listSTARs
    "STAR"
    "NO STARS"
    ((setSTAR >=> warnOrSucceed) . Just)
    "ARRIVAL"
    arrivalView


selectStarTransition :: MCDU ()
selectStarTransition =
  selectWith
    listStarTransitions
    "TRANSITION"
    "NO TRANSITIONS"
    ((setStarTransition >=> warnOrSucceed) . Just)
    "ARRIVAL"
    arrivalView


selectApproach :: MCDU ()
selectApproach =
  selectWith
    listApproaches
    "APPROACH"
    "NO APPROACHES"
    ((setApproach >=> warnOrSucceed) . Just)
    "ARRIVAL"
    arrivalView

selectApproachTransition :: MCDU ()
selectApproachTransition =
  selectWith
    listApproachTransitions
    "APPR TRANS"
    "NO TRANSITIONS"
    ((setApproachTransition >=> warnOrSucceed) . Just)
    "ARRIVAL"
    arrivalView
