{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.FGNasal
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.Trans

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.QQ (s)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.State
import Control.Monad
import Text.Printf
import Data.Text.Encoding
import Data.Maybe
import Control.Exception
import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data FPLeg =
  FPLeg
    { legName :: Text
    , legHeading :: Maybe Double
    , legDist :: Maybe Double
    , legRouteDist :: Maybe Double
    , legRemainingDist :: Maybe Double
    , legSpeed :: Maybe Double
    , legSpeedType :: Maybe Text
    , legAlt :: Maybe Double
    , legAltType :: Maybe Text
    , legParent :: Maybe Text
    , legRole :: Maybe Text
    , legIsDiscontinuity :: Bool
    }

instance FromNasal FPLeg where
  fromNasal n =
    FPLeg
      <$> fromNasalField "name" n
      <*> fromNasalFieldMaybe "hdg" n
      <*> fromNasalFieldMaybe "ldist" n
      <*> fromNasalFieldMaybe "cdist" n
      <*> fromNasalFieldMaybe "rdist" n
      <*> fromNasalFieldMaybe "spd" n
      <*> fromNasalFieldMaybe "spdty" n
      <*> fromNasalFieldMaybe "alt" n
      <*> fromNasalFieldMaybe "altty" n
      <*> fromNasalFieldMaybe "p" n
      <*> fromNasalFieldMaybe "role" n
      <*> (fromMaybe False <$> fromNasalFieldMaybe "disc" n)

formatDistance :: Double -> String
formatDistance dist
  | dist < 10
  = printf "%4.1fNM" dist
  | otherwise
  = printf "%4.0fNM" dist

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

data WaypointCandidate =
  WaypointCandidate
    { wpID :: Text
    , wpType :: Text
    , wpName :: Text
    , wpDistance :: Double
    , wpBearing :: Double
    , wpValue :: NasalValue
    }
    deriving (Show)

releaseWaypointCandidate :: WaypointCandidate -> MCDU ()
releaseWaypointCandidate candidate =
  fgCallNasal "release" [wpValue candidate]

acquireWaypointCandidate :: WaypointCandidate -> MCDU ()
acquireWaypointCandidate candidate =
  fgCallNasal "acquire" [wpValue candidate]

instance FromNasal WaypointCandidate where
  fromNasal nv =
    WaypointCandidate
      <$> fromNasalField "id" nv
      <*> fromNasalField "type" nv
      <*> fromNasalField "name" nv
      <*> fromNasalField "distance" nv
      <*> fromNasalField "bearing" nv
      <*> fromNasalField "wp" nv

insertDirect :: Maybe WaypointCandidate -> WaypointCandidate -> MCDU Bool
insertDirect fromWPMay toWP = do
  let nasalFunc = case wpType toWP of
        "leg" -> "fms.insertDirectFP"
        _ -> "fms.insertDirect"
  result <- (fgCallNasal nasalFunc (wpValue toWP, wpValue <$> fromWPMay) :: MCDU (Maybe ByteString))
  forM_ result $ \err -> scratchWarn err
  return $ isNothing result

resolveLeg :: ByteString -> (Maybe WaypointCandidate -> MCDU ()) -> MCDU ()
resolveLeg name cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getFPLegIndex" [name]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

getLeg :: Int -> (Maybe WaypointCandidate -> MCDU ()) -> MCDU ()
getLeg index cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getWaypoint" [index]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

resolveWaypoint :: ByteString -> ByteString -> (Maybe WaypointCandidate -> MCDU ()) -> MCDU ()
resolveWaypoint returnTitle name cont = do
  candidates :: [WaypointCandidate] <- fgCallNasal "fms.findWaypoint" [name]
  case candidates of
    [] -> do
      scratchWarn "NO WPT"
      cont Nothing
    [candidate] -> do
      cont (Just candidate)
      releaseWaypointCandidate candidate
    _ -> do
      mapM_ acquireWaypointCandidate candidates
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

loadDirectToView :: Maybe ByteString -> MCDU ()
loadDirectToView Nothing = do
  loadDirectToViewWith Nothing Nothing
loadDirectToView (Just toName) = do
  resolveWaypoint "NO WPT" toName (loadDirectToViewWith Nothing)

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
        resolveWaypoint "DIRECT TO" n $ \wpMay -> do
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
            if done then
              loadView fplView
            else
              reload

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

fplView :: MCDUView
fplView = defView
  { mcduViewTitle = "ACT FPL"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = fplViewLoad
  }

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


fplViewLoad :: MCDU ()
fplViewLoad = withFGView $ \conn -> do
  let legsPerPage = numLSKs - 1
  curPage <- gets (mcduViewPage . mcduView)

  (groundspeed :: Double) <- max 100 <$> callNasalFunc conn "fms.getGroundspeed" ()
  (utcMinutes :: Double) <- callNasalFunc conn "fms.getUTCMinutes" ()

  planSize <- callNasalFunc conn "fms.getFlightplanSize" ()
  currentLeg <- callNasalFunc conn "fms.getCurrentLeg" ()

  curLegs <- callNasalFunc conn "fms.getFlightplanLegs" (legsPerPage, curPage, fromMaybe 1 currentLeg - 1)
  flightplanModified <- callNasalFunc conn "fms.hasFlightplanModifications" ()
  let legsDropped = curPage * legsPerPage
  let numPages = (planSize - fromMaybe 0 currentLeg + legsPerPage) `div` legsPerPage

  let putWaypoint :: Int -> Maybe ByteString -> MCDU Bool
      putWaypoint n Nothing = do
        callNasalFunc conn "fms.deleteWaypoint" [n]
      putWaypoint n (Just targetWPName) = do
        resolveWaypoint "FPL" targetWPName $ \toWPMay -> do
          getLeg n $ \fromWPMay -> do
            toIndexMay <- fgCallNasalDef Nothing "fms.findFPWaypoint" ((), wpValue <$> toWPMay)
            fromIndexMay <- fgCallNasalDef Nothing "fms.findFPWaypoint" ((), wpValue <$> fromWPMay)
            case (toIndexMay :: Maybe Int, fromIndexMay :: Maybe Int) of
              (Nothing, Nothing) -> do
                scratchWarn "INVALID"
                mapM_ releaseWaypointCandidate toWPMay
                mapM_ releaseWaypointCandidate fromWPMay
                reloadView
              (Just toIndex, Just fromIndex) ->
                if (toIndex < fromIndex) then
                  loadDirectToViewWith toWPMay fromWPMay
                else
                  loadDirectToViewWith fromWPMay toWPMay
              _ ->
                loadDirectToViewWith fromWPMay toWPMay
        return True
      getWaypoint n = do
        callNasalFunc conn "fms.getWaypointName" [n]

      putRestrictions :: Int -> Maybe ByteString -> MCDU Bool
      putRestrictions n Nothing = do
        err1 <- fgCallNasalDef Nothing "fms.setLegSpeed" (n, (), "" :: Text)
        case err1 of
          Just e -> do
            scratchWarn e
            return False
          Nothing -> do
            err2 <- fgCallNasalDef Nothing "fms.setLegAltitude" (n, (), "" :: Text)
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
                      fgCallNasalDef Nothing "fms.setLegSpeed" (n, speedMay, speedType)
            case err1 of
              Just e -> do
                scratchWarn e
                return False
              Nothing -> do
                err2 <- if altType == "keep" then
                          return Nothing
                        else
                          fgCallNasalDef Nothing "fms.setLegAltitude" (n, altMay, altType)
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
              mcduPrint 6 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legRemainingDist leg)
              mcduPrint 0 (n * 2 + 2) color "---- DISCONTINUITY ----"
            else if isCurrent then do
              mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
              mcduPrint 6 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legRemainingDist leg)
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            else do
              mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
              mcduPrint 6 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legDist leg)
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
rteViewLoad = withFGView $ \conn -> do
  departureMay <- getDeparture
  destinationMay <- getDestination
  callsign <- lift getCallsign
  flightplanModified <- callNasalFunc conn "fms.hasFlightplanModifications" ()
  modifyView $ \v -> v
    { mcduViewNumPages = 1
    , mcduViewTitle = if flightplanModified then "MOD RTE" else "ACT RTE"
    , mcduViewLSKBindings = Map.fromList $
        [ (LSKL 0, ("", do
                    scratchInteract
                      setDeparture
                      getDeparture
                    reloadView))
        , (LSKL 2, ("CLEAR", fgCallNasalBool "fms.clearFlightplan" () >> reloadView))
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
      sidValid <- fgRunNasalBool
                    [s| var fp = flightplan();
                        if (fp.departure == nil) return 1;
                        if (fp.sid == nil) return 1;
                        if (fp.departure_runway == nil) return 1;
                        var runways = fp.sid.runways;
                        var runway = fp.departure_runway.id;
                        return contains(runways, runway);
                      |]
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
                          setSID
                        reloadView))
            , (LSKL 2, ("", do
                        scratchInteractOrSelect
                          selectSidTransition
                          setSidTransition
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
      starValid <- fgRunNasalBool
                    [s| var fp = flightplan();
                        if (fp.destination == nil) return 1;
                        if (fp.star == nil) return 1;
                        if (fp.destination_runway == nil) return 1;
                        var runways = fp.star.runways;
                        var runway = fp.destination_runway.id;
                        return contains(runways, runway);
                      |]
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
                          setApproach
                        reloadView))
            , (LSKL 2, ("", do
                        scratchInteractOrSelect
                          selectSTAR
                          setSTAR
                        reloadView))
            , (LSKR 1, ("", do
                        scratchInteractOrSelect
                          selectApproachTransition
                          setApproachTransition
                        reloadView))
            , (LSKR 2, ("", do
                        scratchInteractOrSelect
                          selectStarTransition
                          setStarTransition
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

withFGView :: (FGFSConnection -> MCDU ()) -> MCDU ()
withFGView go = do
  connMay <- gets mcduFlightgearConnection
  case connMay of
    Nothing -> fgErrorView "NO CONNECTION"
    Just conn -> do
      go conn `mcduCatches` handlers
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

selectWith :: Text
           -> ByteString
           -> ByteString
           -> (ByteString -> MCDU Bool)
           -> ByteString
           -> MCDUView
           -> MCDU ()
selectWith nasalFunc selectTitle warnMsg handleValue returnTitle returnView = do
  itemsMay <- fgCallNasal nasalFunc ()
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

setDeparture :: Maybe ByteString -> MCDU Bool
setDeparture icao = do
  debugPrint $ colorize 0 $ "setDeparture: " <> (Text.pack . show $ icao)
  fgCallNasalBool "fms.setDeparture" [icao]

getDeparture :: MCDU (Maybe ByteString)
getDeparture = fgCallNasal "fms.getDeparture" ()

setDepartureRunway :: Maybe ByteString -> MCDU Bool
setDepartureRunway rwyID =
  fgCallNasalBool "fms.setDepartureRunway" [rwyID]

getDepartureRunway :: MCDU (Maybe ByteString)
getDepartureRunway =
  fgCallNasal "fms.getDepartureRunway" ()

selectDepartureRunway :: MCDU ()
selectDepartureRunway =
  selectWith
    "fms.listDepartureRunways"
    "RUNWAY"
    "NO RUNWAYS"
    (setDepartureRunway . Just)
    "DEPARTURE"
    departureView

setSID :: Maybe ByteString -> MCDU Bool
setSID sidID = do
  fgCallNasal "fms.setSID" [sidID] >>= warnOrSucceed

selectSID :: MCDU ()
selectSID =
  selectWith
    "fms.listSIDs"
    "SID"
    "NO SIDS"
    (setSID . Just)
    "DEPARTURE"
    departureView


getSID :: MCDU (Maybe ByteString)
getSID = fgCallNasal "fms.getSID" ()


setSidTransition :: Maybe ByteString -> MCDU Bool
setSidTransition sidID = do
  fgCallNasal "fms.setSidTransition" [sidID] >>= warnOrSucceed

selectSidTransition :: MCDU ()
selectSidTransition =
  selectWith
    "fms.listSidTransitions"
    "TRANSITION"
    "NO TRANSITIONS"
    (setSidTransition . Just)
    "DEPARTURE"
    departureView


getSidTransition :: MCDU (Maybe ByteString)
getSidTransition = fgCallNasal "fms.getSidTransition" ()

setDestination :: Maybe ByteString -> MCDU Bool
setDestination icao = fgCallNasalBool "fms.setDestination" [icao]

getDestination :: MCDU (Maybe ByteString)
getDestination = fgCallNasal "fms.getDestination" ()


getDestinationRunway :: MCDU (Maybe ByteString)
getDestinationRunway =
  fgCallNasal "fms.getDestinationRunway" ()

setDestinationRunway :: Maybe ByteString -> MCDU Bool
setDestinationRunway rwyID =
  fgCallNasalBool "fms.setDestinationRunway" [rwyID]

selectDestinationRunway :: MCDU ()
selectDestinationRunway =
  selectWith
    "fms.listDestinationRunways"
    "RUNWAY"
    "NO RUNWAYS"
    (setDestinationRunway . Just)
    "ARRIVAL"
    arrivalView

setSTAR :: Maybe ByteString -> MCDU Bool
setSTAR starID = do
  fgCallNasal "fms.setSTAR" [starID] >>= warnOrSucceed

selectSTAR :: MCDU ()
selectSTAR =
  selectWith
    "fms.listSTARs"
    "STAR"
    "NO STARS"
    (setSTAR . Just)
    "ARRIVAL"
    arrivalView


getSTAR :: MCDU (Maybe ByteString)
getSTAR = fgCallNasal "fms.getSTAR" ()


setStarTransition :: Maybe ByteString -> MCDU Bool
setStarTransition starID = do
  fgCallNasal "fms.setStarTransition" [starID] >>= warnOrSucceed

selectStarTransition :: MCDU ()
selectStarTransition =
  selectWith
    "fms.listStarTransitions"
    "TRANSITION"
    "NO TRANSITIONS"
    (setStarTransition . Just)
    "ARRIVAL"
    arrivalView


getStarTransition :: MCDU (Maybe ByteString)
getStarTransition = fgCallNasal "fms.getStarTransition" ()


setApproach :: Maybe ByteString -> MCDU Bool
setApproach approachID = do
  fgCallNasal "fms.setApproach" [approachID] >>= warnOrSucceed

selectApproach :: MCDU ()
selectApproach =
  selectWith
    "fms.listApproaches"
    "APPROACH"
    "NO APPROACHES"
    (setApproach . Just)
    "ARRIVAL"
    arrivalView

getApproach :: MCDU (Maybe ByteString)
getApproach = fgCallNasal "fms.getApproach" ()


setApproachTransition :: Maybe ByteString -> MCDU Bool
setApproachTransition approachID = do
  fgCallNasal "fms.setApproachTransition" [approachID] >>= warnOrSucceed

selectApproachTransition :: MCDU ()
selectApproachTransition =
  selectWith
    "fms.listApproachTransitions"
    "APPR TRANS"
    "NO TRANSITIONS"
    (setApproachTransition . Just)
    "ARRIVAL"
    arrivalView

getApproachTransition :: MCDU (Maybe ByteString)
getApproachTransition = fgCallNasal "fms.getApproachTransition" ()

cancelFlightplanEdits :: MCDU ()
cancelFlightplanEdits = fgCallNasal "fms.cancelFlightplanEdits" ()

commitFlightplanEdits :: MCDU ()
commitFlightplanEdits = fgCallNasal "fms.commitFlightplanEdits" ()
