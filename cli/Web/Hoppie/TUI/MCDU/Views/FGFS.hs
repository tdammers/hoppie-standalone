{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.StringUtil
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

data FPLeg =
  FPLeg
    { legName :: Text
    , legHeading :: Maybe Double
    , legDist :: Maybe Double
    , legRouteDist :: Maybe Double
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
      <*> fromNasalField "heading" n
      <*> fromNasalField "leg_dist" n
      <*> fromNasalField "route_dist" n
      <*> fromNasalField "speed" n
      <*> fromNasalField "speed_type" n
      <*> fromNasalField "alt" n
      <*> fromNasalField "alt_type" n
      <*> fromNasalField "parent" n
      <*> fromNasalField "role" n
      <*> (fromMaybe False <$> fromNasalField "discontinuity" n)

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

formatETA :: Double -> String
formatETA eta =
  let (minutesRaw :: Int) = floor eta `mod` (24 * 60)
      (hours, minutes) = minutesRaw `divMod` 60
  in printf "%02i%02i" hours minutes

fplView :: MCDUView
fplView = defView
  { mcduViewTitle = "ACT FPL"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = fplViewLoad
  }

withFGNasal_ :: (FGFSConnection -> MCDU ()) -> MCDU ()
withFGNasal_ = withFGNasalDef ()

withFGNasalBool :: (FGFSConnection -> MCDU Bool) -> MCDU Bool
withFGNasalBool = withFGNasalDef False

withFGNasal :: Monoid a => (FGFSConnection -> MCDU a) -> MCDU a
withFGNasal = withFGNasalDef mempty

withFGNasalDef :: forall a. a -> (FGFSConnection -> MCDU a) -> MCDU a
withFGNasalDef defval action = do
  connMay <- gets mcduFlightgearConnection
  case connMay of
    Nothing ->
      handleError "NO CONNECTION" Nothing
    Just conn -> do
      liftIO (loadNasalLibrary conn "fms" "nasal/flightplan.nas")
      action conn `mcduCatches` handlers
  where

    handleError :: ByteString -> Maybe String -> MCDU a
    handleError scratchTxt logTxt = do
      forM_ logTxt $ debugPrint . colorize red . Text.pack
      scratchWarn scratchTxt
      return defval

    handlers :: [MCDUHandler a]
    handlers =
      [ MCDUHandler $ \case
          NasalUnexpected expected found -> do
            handleError "SERVER ERROR" . Just $
              "Nasal value error: expected " <> expected <> ", but found " <> found
          NasalMissingKey key -> do
            handleError "SERVER ERROR" . Just $
              "Nasal value error: map key " <> key <> "missing"
      , MCDUHandler $ \case
          NasalRuntimeError msg stackTrace -> do
            handleError "SERVER ERROR" . Just $
                "Nasal runtime error:" <> msg <> "\n" <>
                unlines
                  [ fromMaybe "?" fileMay <> ":" <> maybe "-" show lineMay
                  | (fileMay, lineMay) <- stackTrace
                  ]
      , MCDUHandler $ \case
          PropJSONDecodeError raw err -> do
            handleError "JSON ERROR" . Just $ "JSON decoder error: " <> err <> "\n" <> raw
      , MCDUHandler $ \(e :: SomeException) -> do
            handleError "ERROR" . Just $ "Error:\n" <> show e
      ]


fgCallNasal :: forall a r. (ToNasal a, FromNasal r, Monoid r) => Text -> a -> MCDU r
fgCallNasal = fgCallNasalDef mempty

fgCallNasalBool :: forall a. (ToNasal a) => Text -> a -> MCDU Bool
fgCallNasalBool = fgCallNasalDef False

fgCallNasalDef :: forall a r. (ToNasal a, FromNasal r) => r -> Text -> a -> MCDU r
fgCallNasalDef defval func args =
  withFGNasalDef defval $ \conn -> do
    loadNasalLibrary conn "fms" "nasal/flightplan.nas"
    callNasalFunc conn func args

fgRunNasal :: forall r. (FromNasal r, Monoid r) => Text -> MCDU r
fgRunNasal = fgRunNasalDef mempty

fgRunNasalBool :: Text -> MCDU Bool
fgRunNasalBool = fgRunNasalDef False

fgRunNasalDef :: forall a. FromNasal a => a -> Text -> MCDU a
fgRunNasalDef defval script = do
  withFGNasalDef defval $ \conn -> do
    loadNasalLibrary conn "fms" "nasal/flightplan.nas"
    runNasal conn script

fplViewLoad :: MCDU ()
fplViewLoad = withFGView $ \conn -> do
  (groundspeed :: Double) <- max 100 <$> callNasalFunc conn "fms.getGroundspeed" ()
  (utcMinutes :: Double) <- callNasalFunc conn "fms.getUTCMinutes" ()
  legs' <- callNasalFunc conn "fms.getFlightplanLegs" ()
  currentLeg <- callNasalFunc conn "fms.getCurrentLeg" ()
  flightplanModified <- callNasalFunc conn "fms.hasFlightplanModifications" ()
  let legs = case currentLeg of
                Nothing -> legs'
                Just i -> drop i legs'
  curPage <- gets (mcduViewPage . mcduView)
  let legsDropped = curPage * 5
  let numPages = (length legs + 4) `div` 5
  let curLegs = take 5 . drop legsDropped $ legs
  modifyView $ \v -> v
    { mcduViewNumPages = numPages
    , mcduViewTitle = if flightplanModified then "MOD FPL" else "ACT FPL"
    , mcduViewDraw = do
        when (null legs) $ do
          mcduPrintC (screenW `div` 2) (screenH `div` 2) white "NO FPL"
        zipWithM_
          (\n leg -> do
            let isCurrent = n + legsDropped == 0
                color = if isCurrent then
                          magenta
                        else if legIsDiscontinuity leg then
                          white
                        else if (legRole leg == Just "missed") then
                          cyan
                        else
                          green
            forM_ (legRouteDist leg) $ \routeDist -> do
              when (groundspeed > 40) $ do
                let eta = utcMinutes + routeDist / groundspeed * 60
                mcduPrint (screenW - 6) (n * 2 + 1) color (BS8.pack $ formatETA eta <> "z")
            mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
            mcduPrint 9 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legDist leg)
            mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
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
rteViewLoad = do
  departureMay <- getDeparture
  destinationMay <- getDestination
  callsign <- lift getCallsign
  modifyView $ \v -> v
    { mcduViewNumPages = 1
    , mcduViewLSKBindings = Map.fromList
        [ (0, ("", do
                    scratchInteract
                      setDeparture
                      getDeparture
                    reloadView))
        , (3, ("DEPARTURE", loadView departureView))

        , (5, ("", do
                    scratchInteract
                      setDestination
                      getDestination
                    reloadView))
        , (6, ("", do
                    scratchInteract
                      (maybe (return False) (\c -> lift (setCallsign c) >> return True))
                      (Just <$> lift getCallsign)
                    reloadView))
        , (8, ("ARRIVAL", loadView arrivalView))
        ]
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
  , mcduViewNumPages = (length options + 7) `div` 8
  , mcduViewLSKBindings = mempty
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      let curOptions = take 8 . drop (curPage * 8) $ options
      modifyView $ \v -> v {
        mcduViewLSKBindings = Map.fromList
          [ (n, (option, handleResult (Just option)))
          | (n, option) <- zip [0,5,1,6,2,7,3,8] curOptions
          ]
      }
      addLskBinding 4 returnLabel (handleResult Nothing)
  }

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
            [ (0, ("", do
                        scratchInteractOrSelect
                          selectDepartureRunway
                          setDepartureRunway
                        reloadView))
            , (1, ("", do
                        scratchInteractOrSelect
                          selectSID
                          setSID
                        reloadView))
            , (2, ("", do
                        scratchInteractOrSelect
                          selectSidTransition
                          setSidTransition
                        reloadView))
            , (4, ("RTE", loadView rteView))
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
            [ (0, ("", do
                        scratchInteractOrSelect
                          selectDestinationRunway
                          setDestinationRunway
                        reloadView))
            , (1, ("", do
                        scratchInteractOrSelect
                          selectApproach
                          setApproach
                        reloadView))
            , (2, ("", do
                        scratchInteractOrSelect
                          selectSTAR
                          setSTAR
                        reloadView))
            , (6, ("", do
                        scratchInteractOrSelect
                          selectApproachTransition
                          setApproachTransition
                        reloadView))
            , (7, ("", do
                        scratchInteractOrSelect
                          selectStarTransition
                          setStarTransition
                        reloadView))
            , (4, ("RTE", loadView rteView))
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
      liftIO (loadNasalLibrary conn "fms" "nasal/flightplan.nas")
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

warnOrSucceed :: Maybe ByteString -> MCDU Bool
warnOrSucceed Nothing = return True
warnOrSucceed (Just e) = do
  scratchWarn e
  return False

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
setSidTransition Nothing = do
  fgRunNasalBool
    [s| flightplan().sid_trans = nil;
        return 1;
      |]
setSidTransition (Just transitionID) = do
  err <- fgRunNasal $
    "var transitionID = " <> encodeNasal (decodeUtf8 transitionID) <> ";\n" <>
    [s| var departure = flightplan().departure;
        if (departure == nil) return "NO DEPARTURE";
        var sid = flightplan().sid;
        if (sid == nil) return "NO SID";
        var transitions = sid.transitions;
        debug.dump(transitions);
        if (!contains(transitions, transitionID)) return "INVALID";
        flightplan().sid_trans = sid.transition(transitionID);
        return nil;
      |]
  case err of
    Nothing ->
      return True
    Just e -> do
      scratchWarn e
      return False

selectSidTransition :: MCDU ()
selectSidTransition = do
  availableSidTransitions <- fgRunNasal
    [s| var departure = flightplan().departure;
        if (departure == nil) return [];
        var sid = flightplan().sid;
        if (sid == nil)
          return [];
        else
          return sid.transitions;
      |]
  case availableSidTransitions of
    [] ->
      scratchWarn "NO TRANSITIONS"
    transitions -> do
      let handleResult Nothing = loadView departureView
          handleResult (Just transition) = do
            setSidTransition (Just transition) >>= \case
              True ->
                loadView departureView
              False -> do
                return ()
      loadView (selectView "SELECT TRANSITION" transitions "DEPARTURE" handleResult)

getSidTransition :: MCDU (Maybe ByteString)
getSidTransition = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.sid_trans == nil) ? nil : fp.sid_trans.id);
      |]

setDeparture :: Maybe ByteString -> MCDU Bool
setDeparture Nothing = do
  fgRunNasalBool
    [s| flightplan().departure = nil;
        return 1;
      |]
setDeparture (Just icao) = do
  fgRunNasalBool $
    "var icao = " <> encodeNasal (decodeUtf8 icao) <> ";\n" <>
    [s| var airports = findAirportsByICAO(icao);
        if (size(airports) == 0) return 0;
        flightplan().departure = airports[0];
        return 1;
      |]

getDeparture :: MCDU (Maybe ByteString)
getDeparture = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.departure == nil) ? nil : fp.departure.id);
      |]

setDestination :: Maybe ByteString -> MCDU Bool
setDestination Nothing = do
  fgRunNasalBool
    [s| flightplan().destination = nil;
        return 1;
      |]
setDestination (Just icao) = do
  fgRunNasalBool $
    "var icao = " <> encodeNasal (decodeUtf8 icao) <> ";\n" <>
    [s| var airports = findAirportsByICAO(icao);
        if (size(airports) == 0) return 0;
        flightplan().destination = airports[0];
        return 1;
      |]
getDestination :: MCDU (Maybe ByteString)
getDestination = do
  fgRunNasal
    [s| var fp = flightplan();
        if (fp == nil) return nil;
        return ((fp.destination == nil) ? nil : fp.destination.id);
      |]

setDestinationRunway :: Maybe ByteString -> MCDU Bool
setDestinationRunway Nothing = do
  fgRunNasalBool
    [s| var destinationAirport = flightplan().destination;
        flightplan().destination = nil;
        flightplan().destination = destinationAirport;
        return 1;
      |]
setDestinationRunway (Just rwyID) = do
  fgRunNasalBool $
    "var runwayID = " <> encodeNasal (decodeUtf8 rwyID) <> ";\n" <>
    [s| var destination = flightplan().destination;
        if (destination == nil) return 0;
        var runway = destination.runways[runwayID];
        if (runway == nil) return 0;
        flightplan().destination_runway = runway;
        return 1;
      |]

selectDestinationRunway :: MCDU ()
selectDestinationRunway = do
  runwaysMay <- fgRunNasal
    [s| var runways = flightplan().destination.runways;
        return keys(runways);
      |]
  case runwaysMay of
    [] -> do
      scratchWarn "NO RUNWAYS"
    runways -> do
      let handleResult Nothing = loadView arrivalView
          handleResult (Just rwy) = do
            setDestinationRunway (Just rwy) >>= \case
              True ->
                loadView arrivalView
              False -> do
                return ()
      loadView (selectView "SELECT RUNWAY" runways "ARRIVAL" handleResult)
    

getDestinationRunway :: MCDU (Maybe ByteString)
getDestinationRunway = do
  fgRunNasal
    [s| var fp = flightplan();
        var rwyID = ((fp.destination_runway == nil) ? nil : fp.destination_runway.id);
        return rwyID
      |]

setSTAR :: Maybe ByteString -> MCDU Bool
setSTAR Nothing = do
  fgRunNasalBool
    [s| flightplan().star = nil;
        return 1;
      |]
setSTAR (Just starID) = do
  err <- fgRunNasal $
    "var starID = " <> encodeNasal (decodeUtf8 starID) <> ";\n" <>
    [s| var destination = flightplan().destination;
        if (destination == nil) return "NO ARRIVAL";
        var star = destination.getStar(starID);
        if (star == nil) return "INVALID";
        flightplan().star = star;
        runways = flightplan().star.runways;
        if (size(runways) == 1) {
          var runway = destination.runways[runways[0]];
          if (runway == nil) {
            return "INVALID RWY";
          }
          flightplan().destination_runway = runway;
        }
        elsif (size(runways) > 1) {
          var runway = flightplan().destination_runway;
          if (runway != nil and !contains(runways, runway.id)) {
            flightplan().destination = nil;
            flightplan().destination = destination;
          }
        }
        return nil;
      |]
  case err of
    Nothing ->
      return True
    Just e -> do
      scratchWarn e
      return False

selectSTAR :: MCDU ()
selectSTAR = do
  availableSTARs <- fgRunNasal
    [s| var destination = flightplan().destination;
        if (destination == nil) return [];
        var runway = flightplan().destination_runway;
        if (runway == nil)
          return destination.stars()
        else
          return destination.stars(runway.id);
      |]
  case availableSTARs of
    [] ->
      scratchWarn "NO STARS"
    stars -> do
      let handleResult Nothing = loadView arrivalView
          handleResult (Just star) = do
            setSTAR (Just star) >>= \case
              True ->
                loadView arrivalView
              False -> do
                return ()
      loadView (selectView "SELECT STAR" stars "ARRIVAL" handleResult)

getSTAR :: MCDU (Maybe ByteString)
getSTAR = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.star == nil) ? nil : fp.star.id);
      |]

setApproach :: Maybe ByteString -> MCDU Bool
setApproach Nothing = do
  fgRunNasalBool
    [s| flightplan().approach = nil;
        return 1;
      |]
setApproach (Just approachID) = do
  err <- fgRunNasal $
    "var approachID = " <> encodeNasal (decodeUtf8 approachID) <> ";\n" <>
    [s| var destination = flightplan().destination;
        if (destination == nil) return "NO ARRIVAL";
        var approach = destination.getIAP(approachID);
        if (approach == nil) return "INVALID";
        flightplan().approach = approach;
        runways = flightplan().approach.runways;
        if (size(runways) == 1) {
          var runway = destination.runways[runways[0]];
          if (runway == nil) {
            return "INVALID RWY";
          }
          flightplan().destination_runway = runway;
        }
        elsif (size(runways) > 1) {
          var runway = flightplan().destination_runway;
          if (runway != nil and !contains(runways, runway.id)) {
            flightplan().destination = nil;
            flightplan().destination = destination;
          }
        }
        return nil;
      |]
  case err of
    Nothing ->
      return True
    Just e -> do
      scratchWarn e
      return False

selectApproach :: MCDU ()
selectApproach = do
  availableApproaches <- fgRunNasal
    [s| var destination = flightplan().destination;
        if (destination == nil) return [];
        var runway = flightplan().destination_runway;
        if (runway == nil)
          return destination.getApproachList()
        else
          return destination.getApproachList(runway.id);
      |]
  case availableApproaches of
    [] ->
      scratchWarn "NO APPROACHES"
    approaches -> do
      let handleResult Nothing = loadView arrivalView
          handleResult (Just approach) = do
            setApproach (Just approach) >>= \case
              True ->
                loadView arrivalView
              False -> do
                return ()
      loadView (selectView "SELECT APPROACH" approaches "ARRIVAL" handleResult)

getApproach :: MCDU (Maybe ByteString)
getApproach = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.approach == nil) ? nil : fp.approach.id);
      |]

setStarTransition :: Maybe ByteString -> MCDU Bool
setStarTransition Nothing = do
  fgRunNasalBool
    [s| flightplan().transition = nil;
        return 1;
      |]
setStarTransition (Just transitionID) = do
  err <- fgRunNasal $
    "var transitionID = " <> encodeNasal (decodeUtf8 transitionID) <> ";\n" <>
    [s| var destination = flightplan().destination;
        if (destination == nil) return "NO DESTINATION";
        var star = flightplan().star;
        if (star == nil) return "NO STAR";
        var transitions = star.transitions;
        debug.dump(transitions);
        if (!contains(transitions, transitionID)) return "INVALID";
        flightplan().star_trans = star.transition(transitionID);
        return nil;
      |]
  case err of
    Nothing ->
      return True
    Just e -> do
      scratchWarn e
      return False

selectStarTransition :: MCDU ()
selectStarTransition = do
  availableTransitions <- fgRunNasal
    [s| var destination = flightplan().destination;
        if (destination == nil) return [];
        var star = flightplan().star;
        if (star == nil)
          return [];
        else
          return star.transitions;
      |]
  case availableTransitions of
    [] ->
      scratchWarn "NO TRANSITIONS"
    transitions -> do
      let handleResult Nothing = loadView arrivalView
          handleResult (Just transition) = do
            setStarTransition (Just transition) >>= \case
              True ->
                loadView arrivalView
              False -> do
                return ()
      loadView (selectView "SELECT TRANSITION" transitions "ARRIVAL" handleResult)

getStarTransition :: MCDU (Maybe ByteString)
getStarTransition = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.star_trans == nil) ? nil : fp.star_trans.id);
      |]

setApproachTransition :: Maybe ByteString -> MCDU Bool
setApproachTransition Nothing = do
  fgRunNasalBool
    [s| flightplan().approach_trans = nil;
        return 1;
      |]
setApproachTransition (Just transitionID) = do
  err <- fgRunNasal $
    "var transitionID = " <> encodeNasal (decodeUtf8 transitionID) <> ";\n" <>
    [s| var destination = flightplan().destination;
        if (destination == nil) return "NO DESTINATION";
        var approach = flightplan().approach;
        if (approach == nil) return "NO APPROACH";
        var transitions = approach.transitions;
        debug.dump(transitions);
        if (!contains(transitions, transitionID)) return "INVALID";
        flightplan().approach_trans = approach.transition(transitionID);
        return nil;
      |]
  case err of
    Nothing ->
      return True
    Just e -> do
      scratchWarn e
      return False

selectApproachTransition :: MCDU ()
selectApproachTransition = do
  availableApproachTransitions <- fgRunNasal
    [s| var destination = flightplan().destination;
        if (destination == nil) return [];
        var approach = flightplan().approach;
        if (approach == nil)
          return [];
        else
          return approach.transitions;
      |]
  case availableApproachTransitions of
    [] ->
      scratchWarn "NO TRANSITIONS"
    transitions -> do
      let handleResult Nothing = loadView arrivalView
          handleResult (Just transition) = do
            setApproachTransition (Just transition) >>= \case
              True ->
                loadView arrivalView
              False -> do
                return ()
      loadView (selectView "SELECT TRANSITION" transitions "ARRIVAL" handleResult)

getApproachTransition :: MCDU (Maybe ByteString)
getApproachTransition = do
  fgRunNasal
    [s| var fp = flightplan();
        return ((fp.approach_trans == nil) ? nil : fp.approach_trans.id);
      |]
