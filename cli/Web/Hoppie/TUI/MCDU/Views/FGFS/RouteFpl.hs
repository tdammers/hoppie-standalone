{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS.RouteFpl
where

import Web.Hoppie.FGFS.FMS as FMS
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.MCDU.Views.FGFS.Common
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex
import Web.Hoppie.Trans

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
import Safe (atMay)
import Text.Printf
import Text.Read (readMaybe)


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
                altStr = BS8.pack $ formatAltitudeCompact (fromMaybe 18000 transAlt) (legAlt leg) (legAltType leg)
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
              mcduPrint (screenW - 6) (n * 2 + 2) color (BS8.pack $ formatAltitude (fromMaybe 18000 transAlt) (legAlt leg) (legAltType leg))

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
