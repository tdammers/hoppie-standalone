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
import Safe (atMay)

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

formatAltitude :: Double -> Maybe Double -> Maybe Text -> String
formatAltitude transAlt (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= transAlt
         -> printf "%5.0f" alt
      () | otherwise
         -> printf "FL%03.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatAltitude _ _ _ = "  ---"

formatAltitudeCompact :: Double -> Maybe Double -> Maybe Text -> String
formatAltitudeCompact transAlt (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= transAlt
         -> printf "%1.0f" alt
      () | otherwise
         -> printf "FL%1.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> ""
      _ -> ""
formatAltitudeCompact _ _ _ = "-"


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

formatEFOB :: Word8 -> Double -> Double -> Double -> Maybe Double -> Colored ByteString
formatEFOB _ _ _ _ Nothing = ""
formatEFOB defcolor finres cont maxFOB (Just efob)
  | maxFOB >= 10000
  , efob >= 1000000
  = colorize color "+++.+"
  | maxFOB >= 10000
  , efob <= -1000000
  = colorize color "---.-"
  | maxFOB >= 10000
  = colorize color . BS8.pack $ printf "%5.1f" (efob / 1000)
  | efob >= 10000
  = colorize color "++++"
  | maxFOB >= 10000
  , efob <= -10000
  = colorize color "----"
  | otherwise
  = colorize color . BS8.pack $ printf "%5.0f" efob
  where
    color
      | efob <= finres
      = red
      | efob <= finres + cont
      = yellow
      | otherwise
      = defcolor

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

mcduResolveWaypoint :: Bool -> ByteString -> ByteString -> (Maybe WaypointCandidate -> MCDU ()) -> MCDU ()
mcduResolveWaypoint includeLegs returnTitle name =
  resolveWaypoint includeLegs name scratchWarn (loadWaypointSelect returnTitle)

loadDirectToView :: Maybe ByteString -> MCDU ()
loadDirectToView Nothing = do
  loadDirectToViewWith Nothing Nothing
loadDirectToView (Just toName) = do
  mcduResolveWaypoint True "NO WPT" toName (loadDirectToViewWith Nothing)

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
        mcduResolveWaypoint True "DIRECT TO" n $ \wpMay -> do
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
  , mcduViewNumPages = 4
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
        aircraftModel <- getFGAircraftType

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

        let setCruiseFL Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseFL = Nothing }
              return True
            setCruiseFL (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitCruiseFL = Just x, perfInitCruiseAlt = Nothing }
                  return True
            getCruiseFL =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseFL pd

        let setCruiseAlt Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseAlt = Nothing }
              return True
            setCruiseAlt (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitCruiseAlt = Just x, perfInitCruiseFL = Nothing }
                  return True
            getCruiseAlt =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseAlt pd

        let setCruiseMach Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseMach = Nothing }
              return True
            setCruiseMach (Just val) = do
              let val' = BS8.unpack val
                  val'' = case val' of
                            '.':_ -> '0' : val'
                            _ -> val'
              case readMaybe val'' of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  let mach = if x < 1.0 then x else x / 100
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseMach = Just mach
                      , perfInitCruiseIAS = Nothing
                      }
                  return True
            getCruiseMach =
              return $ BS8.pack . printf "%3.0f" . (* 100) <$> perfInitCruiseMach pd

        let setCruiseIAS Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseIAS = Nothing }
              return True
            setCruiseIAS (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseIAS = Just x
                      , perfInitCruiseMach = Nothing
                      }
                  return True
            getCruiseIAS =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseIAS pd


        let setCruiseWind Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseWind = Nothing }
              return True
            setCruiseWind (Just val) = do
              let valStr = BS8.unpack val
                  (hdgStr, r) = span (/= '/') valStr
                  spdStr = drop 1 r
              case (readMaybe hdgStr, readMaybe spdStr) of
                (Just hdg, Just spd) ->  do
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseWind = Just (hdg, spd)
                      }
                  return True
                _ -> do
                  scratchWarn "INVALID"
                  return False
            getCruiseWind =
              case perfInitCruiseWind pd of
                Nothing ->
                  return Nothing
                Just (hdg, spd) ->
                  return . Just $ BS8.pack $ printf "%03i/%02i" hdg spd

        let setTransAlt Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitTransAlt = Nothing }
              return True
            setTransAlt (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitTransAlt = Just x }
                  return True
            getTransAlt =
              return $ BS8.pack . printf "%i" <$> perfInitTransAlt pd

        let setTransFL Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitTransFL = Nothing }
              return True
            setTransFL (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitTransFL = Just x }
                  return True
            getTransFL =
              return $ BS8.pack . printf "%i" <$> perfInitTransFL pd

        let confirmInit = do
              setPerfInitData pd
              reloadView

            resetData = do
              getPerfInitData >>= liftIO . writeIORef pdVar
              reloadView

            isModified getter =
              let lhs = getter pd
                  rhs = getter pdStored
              in
                lhs /= rhs

            isModifiedF getter =
              let lhs = getter pd
                  rhs = getter pdStored
              in
                case (lhs, rhs) of
                  (Nothing, Nothing) -> False
                  (Just l, Just r) ->
                    abs (r - l) > 0.001
                  _ -> True

            fieldColorF getter =
              if isModifiedF getter then
                cyan
              else
                green

            fieldColor getter =
              if isModified getter then
                cyan
              else
                green

            modificationsExist =
              any isModifiedF
                [ perfInitZFW
                , perfInitBlockFuel
                , perfInitMinTakeoffFuel
                , perfInitContingencyFuel
                , perfInitReserveFuel
                , perfInitCruiseMach
                ] ||
              any isModified
                [ perfInitCruiseFL
                , perfInitCruiseAlt
                , perfInitCruiseIAS
                , perfInitTransAlt
                , perfInitTransFL
                ] ||
              isModified perfInitCruiseWind ||
              isModified perfInitClimbProfile

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
                  mcduPrint 1 3 white "A/C MODEL"
                  mcduPrint 1 4 green (fromMaybe "----" aircraftModel)

                  mcduPrint 1 5 white "UNITS"
                  mcduPrint 1 6 green (case massUnit of { Kilograms -> "KG"; Pounds -> "LBS" })
                  mcduPrintR (screenW - 1) 5 white "PERF MODE"
                  mcduPrintR (screenW - 1) 6 green "CUR FF/GS"

                  mcduPrint 1 7 white "TRANS ALT"
                  mcduPrintC (screenW `div` 2) 7 white "/"
                  mcduPrintR (screenW - 1) 7 white "TRANS LVL"
                  mcduPrint 1 8 (fieldColor perfInitTransAlt) $
                    maybe "-----" (BS8.pack . printf "%i") (perfInitTransAlt pd)
                  mcduPrintR (screenW - 1) 8 (fieldColor perfInitTransFL) $
                    maybe "-----" (BS8.pack . printf "FL%i") (perfInitTransFL pd)

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
                  , ( LSKL 3, ("", scratchInteract setTransAlt getTransAlt >> reloadView))
                  , ( LSKR 3, ("", scratchInteract setTransFL getTransFL >> reloadView))
                  ]
              }
          1 -> do
            let profile = perfInitClimbProfile pd
                profileSize = length profile

            let formatIASOrMach (IAS i) = show i
                formatIASOrMach (Mach m) = printf "M%02.0f" (m * 100)
                color = if isModified perfInitClimbProfile then cyan else green

            let drawClimbPerf :: Int -> ClimbPerf -> MCDUDraw s ()
                drawClimbPerf n cp = do
                  mcduPrint 1 (2 * n + 2) white $ "CLB" <> BS8.pack (show $ n + 1)
                  mcduPrint 6 (2 * n + 2) color . BS8.pack $
                    printf "FL%03i / %3s / %4i"
                      (climbToFL cp)
                      (formatIASOrMach $ climbSpeed cp)
                      (climbRate cp)

            let setEntry :: Int -> Maybe ByteString -> MCDU Bool
                setEntry n Nothing =
                  if n < profileSize then do
                    liftIO $ writeIORef pdVar $ pd
                      { perfInitClimbProfile = take n profile ++ drop (n + 1) profile }
                    return True
                  else
                    return False
                setEntry n (Just str) =
                  case BS.split (ord8 '/') str of
                    [flStr, spdStr, rocStr] -> do
                      let parseResultMay = do
                            spd <- case BS.take 1 spdStr of
                                     "M" -> do
                                        mn <- readMaybe . BS8.unpack . BS.drop 1 $ spdStr
                                        Mach <$> if mn > 1.0 then
                                          return $ mn / 100
                                        else
                                          return mn

                                     _ -> IAS <$> (readMaybe . BS8.unpack $ spdStr)
                            roc <- readMaybe . BS8.unpack $ rocStr
                            fl <- readMaybe . BS8.unpack $ flStr
                            return $ ClimbPerf spd roc fl
                      case parseResultMay of
                        Nothing -> do
                          scratchWarn "INVALID"
                          return False
                        Just cp -> do
                          liftIO $ writeIORef pdVar $ pd
                            { perfInitClimbProfile = take n profile ++ [cp] ++ drop (n + 1) profile }
                          return True
                    _ -> do
                      scratchWarn "INVALID"
                      return False
                getEntry n =
                  case drop n $ profile of
                    [] -> return Nothing
                    (cp:_) -> return . Just . BS8.pack $
                      printf "%i/%s/%i"
                        (climbToFL cp)
                        (formatIASOrMach $ climbSpeed cp)
                        (climbRate cp)

            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT CLB"
              , mcduViewDraw = do
                  mcduPrint 6 1 white "TO FL / SPD / FPM"
                  zipWithM_ drawClimbPerf [0..4] (perfInitClimbProfile pd)
                  when (profileSize < 4) $
                    mcduPrint 6 (profileSize * 2 + 2) green "--- / ---- / ---"
                    
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKR n, ("", scratchInteract (setEntry n) (getEntry n) >> reloadView) )
                  | n <- [0..profileSize]
                  ]
              }
          2 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT CRZ"
              , mcduViewDraw = do
                  mcduPrint 1 1 white "CRZ MACH"
                  mcduPrintC (screenW `div` 2) 2 white "<-OR->"
                  mcduPrintR (screenW - 1) 1 white "CRZ IAS"
                  mcduPrint 1 2 (fieldColorF perfInitCruiseMach) $
                    maybe "---" (BS8.pack . printf "%3.2f") (perfInitCruiseMach pd)
                  mcduPrintR (screenW -1) 2 (fieldColor perfInitCruiseIAS) $
                    maybe "---" (BS8.pack . printf "%3i") (perfInitCruiseIAS pd)
                  mcduPrint 1 3 white "CRZ LVL"
                  mcduPrintC (screenW `div` 2) 4 white "<-OR->"
                  mcduPrintR (screenW - 1) 3 white "CRZ ALT"
                  mcduPrint 1 4 (fieldColor perfInitCruiseFL) $
                    maybe "---" (BS8.pack . printf "FL%i") (perfInitCruiseFL pd)
                  mcduPrintR (screenW - 1) 4 (fieldColor perfInitCruiseAlt) $
                    maybe "---" (BS8.pack . printf "%5i") (perfInitCruiseAlt pd)
                  mcduPrint 1 5 white "CRZ WIND"
                  mcduPrint 1 6 (fieldColor perfInitCruiseWind) $
                    maybe "---/--" (\(hdg, spd) -> BS8.pack $ printf "%03i/%02i" hdg spd) (perfInitCruiseWind pd)
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKL 0, ("", scratchInteract setCruiseMach getCruiseMach >> reloadView))
                  , ( LSKR 0, ("", scratchInteract setCruiseIAS getCruiseIAS >> reloadView))
                  , ( LSKL 1, ("", scratchInteract setCruiseFL getCruiseFL >> reloadView))
                  , ( LSKR 1, ("", scratchInteract setCruiseAlt getCruiseAlt >> reloadView))
                  , ( LSKL 2, ("", scratchInteract setCruiseWind getCruiseWind >> reloadView))
                  ]
              }
          3 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT " <> massUnitStr
              , mcduViewDraw = do
                  let formatMass :: Maybe Double -> ByteString
                      formatMass = maybe "-----" (BS8.pack . printf "%7.0f" . (* massFactor))
                  mcduPrintR (screenW - 1) 1 white "ZFW"
                  mcduPrintR (screenW - 1) 2 (fieldColorF perfInitZFW) $ formatMass (perfInitZFW pd)
                  mcduPrint 1 3 white "GAUGE"
                  mcduPrint 1 4 white $ formatMass currentFuel
                  mcduPrintR (screenW - 1) 3 white "BLOCK FUEL"
                  mcduPrintR (screenW - 1) 4 (fieldColorF perfInitBlockFuel) $ formatMass (perfInitBlockFuel pd)
                  mcduPrintR (screenW - 1) 5 white "T/O FUEL"
                  mcduPrintR (screenW - 1) 6 (fieldColorF perfInitMinTakeoffFuel) $ formatMass (perfInitMinTakeoffFuel pd)
                  mcduPrintR (screenW - 1) 7 white "CONT FUEL"
                  mcduPrintR (screenW - 1) 8 (fieldColorF perfInitContingencyFuel) $ formatMass (perfInitContingencyFuel pd)
                  mcduPrintR (screenW - 1) 9 white "RSRV FUEL"
                  mcduPrintR (screenW - 1) 10 (fieldColorF perfInitReserveFuel) $ formatMass (perfInitReserveFuel pd)
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

formatFlightPhase :: FlightPhase -> ByteString
formatFlightPhase f =
  BS8.pack . map (\case { '_' -> ' '; c -> c }) . show $ f

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
      prevPhase = prevFlightPhase =<< (progressFlightPhase <$> progress)
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
                
            mcduPrint 1 5 white "DEST"
            printWP green 6 (progressDestination =<< progress)

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
  currentLeg <- getCurrentLeg

  let offset = max 1 $ fromMaybe 1 currentLeg - 1
  curLegs <- getFlightplanLegs legsPerPage curPage offset
  flightplanModified <- hasFlightplanModifications
  let legsDropped = curPage * legsPerPage + offset
  let numPages = (planSize - fromMaybe 0 currentLeg + legsPerPage) `div` legsPerPage
  massUnit <- gets mcduMassUnit
  let massFactor = case massUnit of
                      Kilograms -> 1
                      Pounds -> 1 / lbs2kg
  perfInit <- getPerfInitData
  fuelCapacity <- (* massFactor) <$> getFuelCapacity
  transAlt <- getTransitionAlt
  let finres = maybe 0 (* massFactor) $ perfInitReserveFuel perfInit
      cont = maybe 0 (* massFactor) $ perfInitContingencyFuel perfInit

  let putWaypoint :: Int -> Maybe ByteString -> MCDU Bool
      putWaypoint n Nothing = do
        forM_ (atMay curLegs n) $ \leg -> do
          forM_ [legIdxFrom leg .. legIdxTo leg] deleteWaypoint
        return True
      putWaypoint n (Just targetWPName) = do
        mcduResolveWaypoint True "FPL" targetWPName $ \case
          Nothing -> do
            loadView fplView
          Just toWP -> do
            forM_ (atMay curLegs n) $ \leg -> do
              getFlightplanLeg (legIdxTo leg) $ \fromWPMay -> do
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
        return $ encodeUtf8 . legName <$> atMay curLegs n

      putRestrictions :: Int -> Maybe ByteString -> MCDU Bool
      putRestrictions n Nothing = do
        fmap (maybe False and) $
          forM (atMay curLegs n) $ \leg -> do
            forM [legIdxFrom leg .. legIdxTo leg] $ \i -> do
              err1 <- setFPLegSpeed i Nothing ""
              case err1 of
                Just e -> do
                  scratchWarn e
                  return False
                Nothing -> do
                  err2 <- setFPLegAltitude i Nothing ""
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
            liftIO . print $ (speedMay, speedType, altMay, altType)
            fmap (maybe False and) $
              forM (atMay curLegs n) $ \leg -> do
                liftIO . print $ leg
                forM [legIdxFrom leg .. legIdxTo leg] $ \i -> do
                  liftIO . print $ i
                  err1 <- if speedType == "keep" then
                            return Nothing
                          else
                            setFPLegSpeed i speedMay speedType
                  case err1 of
                    Just e -> do
                      scratchWarn e
                      return False
                    Nothing -> do
                      err2 <- if altType == "keep" then
                                return Nothing
                              else
                                setFPLegAltitude i altMay altType
                      case err2 of
                        Just e -> do
                          scratchWarn e
                          return False
                        Nothing ->
                          return True
      getRestrictions n = do
        case drop n curLegs of
          (leg:_) -> do
            let spdStr = BS8.pack $ formatSpeedCompact (legSpeed leg) (legSpeedType leg)
                altStr = BS8.pack $ formatAltitudeCompact transAlt (legAlt leg) (legAltType leg)
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
                (putWaypoint n)
                (getWaypoint n)
              reloadView
          ))
        | n <- [0 .. legsPerPage ]
        ]
        ++
        [ (LSKR n, ("", do
              scratchInteract
                (putRestrictions n)
                (getRestrictions n)
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
            let isCurrent = Just (n + legsDropped) == currentLeg
                isPrevious = Just (n + legsDropped + 1) == currentLeg
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
              mcduPrintColored 13 (n * 2 + 1) (formatEFOB color finres cont fuelCapacity . fmap (* massFactor) $ legEFOB leg)
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            unless (legIsDiscontinuity leg) $ do
              mcduPrint (screenW - 11) (n * 2 + 2) color (BS8.pack $ formatSpeed (legSpeed leg) (legSpeedType leg))
              mcduPrint (screenW - 7) (n * 2 + 2) color "/"
              mcduPrint (screenW - 6) (n * 2 + 2) color (BS8.pack $ formatAltitude transAlt (legAlt leg) (legAltType leg))

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
  let (numRoutePages, curRouteLegs) = paginateWithHeadroom 1 6 (curPage - 1) routeLegs
      numPages = numRoutePages + 1
      numLegs = length routeLegs

  liftIO $ mapM_ print curRouteLegs

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
              loadViewAtPage ((numLegs - 1)`div` 6 + 1) rteView
              return ()
        if BS.null toStr then do
          mcduResolveWaypoint False "RTE" viaStr $ \case
            Nothing -> do
              scratchWarn "NO WPT"
              return ()
            Just wp -> do
              result <- appendDirectTo wp
              releaseWaypointCandidate wp
              goResult result
        else do
          result <- appendViaTo viaStr toStr
          goResult result
        return True
      getViaTo :: MCDU (Maybe ByteString)
      getViaTo = return Nothing

      setLeg :: Int -> Maybe ByteString -> MCDU Bool
      setLeg i Nothing = do
        case drop i routeLegs of
          (Just leg:_) -> do
            deleteRouteLeg (routeLegFromIndex leg) (routeLegToIndex leg)
            return True
          _ -> do
            scratchWarn "INVALID"
            return False
      setLeg _ _ = do
        scratchWarn "NOT ALLOWED"
        return False
      getLeg :: Int -> MCDU (Maybe ByteString)
      getLeg i =
        case drop i routeLegs of
          (Just leg:_) -> case routeLegVia leg of
            Nothing -> return . Just $ routeLegTo leg
            Just via -> return . Just $ via <> "." <> routeLegTo leg
          _ -> do
            scratchWarn "INVALID"
            return Nothing

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
          lastLegN = numLegs - offset
      modifyView $ \v -> v
        { mcduViewLSKBindings = mempty
        , mcduViewDraw = do
            mcduPrint  0 1 white "VIA"
            mcduPrint 11 1 white "TO"
            mcduPrintR screenW 1 white "DIST"
            zipWithM_ (\n legMay -> do
              case legMay of
                Just leg -> do
                  if routeLegTo leg == "DISCONTINUITY" then do
                    mcduPrint  0 (n * 2 + 2) white "-DISCONTINUITY-"
                    mcduPrintR screenW (n * 2 + 2) white (BS8.pack $ maybe "" formatDistance $ routeLegDistance leg)
                  else do
                    let via = fromMaybe "DCT" $ routeLegVia leg
                        viaColor = if isNothing (routeLegArrDep leg) then green else cyan
                    if BS.length via > 10 then
                      mcduPrint  0 (n * 2 + 2) viaColor (BS.take 8 via <> "..")
                    else
                      mcduPrint  0 (n * 2 + 2) viaColor via
                    mcduPrint 11 (n * 2 + 2) green (routeLegTo leg)
                    mcduPrintR screenW (n * 2 + 2) green (BS8.pack $ maybe "" formatDistance $ routeLegDistance leg)
                Nothing -> do
                  mcduPrint  1 (n * 2 + 2) green "-----"
                  mcduPrint 11 (n * 2 + 2) green "-----"
                  mcduPrintR screenW (n * 2 + 2) green "-----"
              ) [0,1..] curRouteLegs
        }
      zipWithM_ (\n legMay -> do
          case legMay of
            Just leg -> do
              when (routeLegArrDep leg == Just "sid") $
                addLskBinding (LSKL n) "" (loadView departureView)
              when (routeLegArrDep leg == Just "star") $
                addLskBinding (LSKL n) "" (loadView arrivalView)
              when (isNothing $ routeLegArrDep leg) $
                addLskBinding (LSKR n) "" (scratchInteract (setLeg (n + offset)) (getLeg (n + offset)) >> reloadView)
            Nothing -> do
              addLskBinding
                (LSKR n)
                ""
                (scratchInteract setViaTo getViaTo >> reloadView)
        ) [0,1..] curRouteLegs
      when (flightplanModified && lastLegN + 1 >= 0 && lastLegN + 1 < 6) $ do
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
