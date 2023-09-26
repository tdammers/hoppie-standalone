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
  , mcduViewOnLoad = withFG $ fplViewLoad
  }

fgNasal :: forall a. FromNasal a => FGFSConnection -> a -> Text -> MCDU a
fgNasal conn defval script = do
  debugPrint $ colorize cyan $ "Running script: " <> script
  liftIO (runNasal conn script) `mcduCatches` handlers
  where
    handlers :: [MCDUHandler a]
    handlers =
      [ MCDUHandler $ \case
          NasalUnexpected expected found -> do
            debugPrint $
              colorize red . Text.pack $
              "Nasal value error: expected " <> expected <> ", but found " <> found
            scratchWarn "SERVER ERROR"
            return defval
          NasalMissingKey key -> do
            debugPrint $
              colorize red . Text.pack $
              "Nasal value error: map key " <> key <> "missing"
            scratchWarn "SERVER ERROR"
            return defval
      , MCDUHandler $ \case
          NasalRuntimeError msg fileMay lineMay -> do
            debugPrint $
              colorize red . Text.pack $
                "Nasal runtime error:" <> fromMaybe "?" fileMay <> ":" <> maybe "-" show lineMay <> "\n" <>
                msg
            scratchWarn "SERVER ERROR"
            return defval
      , MCDUHandler $ \(e :: SomeException) -> do
            debugPrint $
              colorize red . Text.pack $
                "Error:\n" <> show e
            scratchWarn "ERROR"
            return defval
      ]


fplViewLoad :: FGFSConnection -> MCDU ()
fplViewLoad conn = do
  (groundspeed :: Double) <- fmap (max 100) . liftIO $ runNasal conn [s| getprop('/velocities/groundspeed-kt'); |]
  (utcMinutes :: Double) <- liftIO $ runNasal conn
    [s| var hour = getprop('/sim/time/utc/hour');
        var minute = getprop('/sim/time/utc/minute');
        var second = getprop('/sim/time/utc/second');
        return (hour * 60 + minute + second / 60);
      |]
  legs' <- liftIO $ runNasal conn
    [s| var fp = flightplan();
        var result = [];
        for (var i = 0; i < fp.getPlanSize(); i += 1) {
          var wp = fp.getWP(i);
          var parent_id = nil;
          if (wp.wp_parent != nil)
            parent_id = wp.wp_parent.id;
          append(result,
            { "name": wp.wp_name
            , "heading": wp.leg_bearing
            , "leg_dist": wp.leg_distance
            , "route_dist": wp.distance_along_route
            , "speed": wp.speed_cstr
            , "speed_type": wp.speed_cstr_type
            , "alt": wp.alt_cstr
            , "alt_type": wp.alt_cstr_type
            , "parent": parent_id
            , "role": wp.wp_role
            , "discontinuity": (wp.wp_type == "discontinuity" or wp.wp_type == "vectors")
            });
        }
        return result;
      |]
  currentLeg <- liftIO $ runNasal conn
    [s| var fp = flightplan();
        return fp.current
      |]
  let legs = case currentLeg of
                Nothing -> legs'
                Just i -> drop i legs'
  curPage <- gets (mcduViewPage . mcduView)
  let legsDropped = curPage * 5
  let numPages = (length legs + 4) `div` 5
  let curLegs = take 5 . drop legsDropped $ legs
  modifyView $ \v -> v
    { mcduViewNumPages = numPages
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
  , mcduViewOnLoad = withFG rteViewLoad
  }


rteViewLoad :: FGFSConnection -> MCDU ()
rteViewLoad conn = do
  let setDeparture :: Maybe ByteString -> MCDU Bool
      setDeparture Nothing = do
        fgNasal conn False $
          [s| flightplan().departure = nil;
              return 1;
            |]
      setDeparture (Just icao) = do
        fgNasal conn False $
          "var icao = " <> encodeNasal (decodeUtf8 icao) <> ";\n" <>
          [s| var airports = findAirportsByICAO(icao);
              if (size(airports) == 0) return 0;
              flightplan().departure = airports[0];
              return 1;
            |]
  let getDeparture :: MCDU (Maybe ByteString)
      getDeparture = do
        fgNasal conn Nothing $
          [s| print("get departure");
              var fp = flightplan();
              return ((fp.departure == nil) ? nil : fp.departure.id);
            |]

  let setDestination :: Maybe ByteString -> MCDU Bool
      setDestination Nothing = do
        fgNasal conn False $
          [s| flightplan().destination = nil;
              return 1;
            |]
      setDestination (Just icao) = do
        fgNasal conn False $
          "var icao = " <> encodeNasal (decodeUtf8 icao) <> ";\n" <>
          [s| var airports = findAirportsByICAO(icao);
              if (size(airports) == 0) return 0;
              flightplan().destination = airports[0];
              return 1;
            |]
  let getDestination :: MCDU (Maybe ByteString)
      getDestination = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.destination == nil) ? nil : fp.destination.id);
            |]

  departureMay <- getDeparture
  destinationMay <- getDestination
  callsign <- lift getCallsign
  modifyView $ \v -> v
    { mcduViewNumPages = 1
    , mcduViewLSKBindings = Map.fromList
        [ (0, ("", scratchInteract setDeparture getDeparture >> reloadView))
        , (3, ("DEPARTURE", loadView departureView))

        , (5, ("", scratchInteract setDestination getDestination >> reloadView))
        , (6, ("", scratchInteract
                      (maybe (return False) (\c -> lift (setCallsign c) >> return True))
                      (Just <$> lift getCallsign)
                   >> reloadView))
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
  , mcduViewOnLoad = withFG departureViewLoad
  }

departureViewLoad :: FGFSConnection -> MCDU ()
departureViewLoad conn = do
  let setRunway :: Maybe ByteString -> MCDU Bool
      setRunway Nothing = do
        fgNasal conn False $
          [s| var departureAirport = flightplan().departure;
              flightplan().departure = nil;
              flightplan().departure = departureAirport;
              return 1;
            |]
      setRunway (Just rwyID) = do
        fgNasal conn False $
          "var runwayID = " <> encodeNasal (decodeUtf8 rwyID) <> ";\n" <>
          [s| var departure = flightplan().departure;
              if (departure == nil) return 0;
              var runway = departure.runways[runwayID];
              if (runway == nil) return 0;
              flightplan().departure_runway = runway;
              return 1;
            |]
      selectRunway = do
        runwaysMay <- fgNasal conn [] $
          [s| var runways = flightplan().departure.runways;
              return keys(runways);
            |]
        case runwaysMay of
          [] -> do
            scratchWarn "NO RUNWAYS"
          runways -> do
            let handleResult Nothing = loadView departureView
                handleResult (Just rwy) = do
                  setRunway (Just rwy) >>= \case
                    True ->
                      loadView departureView
                    False -> do
                      return ()
            loadView (selectView "SELECT RUNWAY" runways "DEPARTURE" handleResult)
          

  let getRunway :: MCDU (Maybe ByteString)
      getRunway = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              var rwyID = ((fp.departure_runway == nil) ? nil : fp.departure_runway.id);
              return rwyID
            |]

  let setSID :: Maybe ByteString -> MCDU Bool
      setSID Nothing = do
        fgNasal conn False $
          [s| flightplan().sid = nil;
              return 1;
            |]
      setSID (Just sidID) = do
        err <- fgNasal conn Nothing $
          "var sidID = " <> encodeNasal (decodeUtf8 sidID) <> ";\n" <>
          [s| var departure = flightplan().departure;
              if (departure == nil) return "NO DEPARTURE";
              var sid = departure.getSid(sidID);
              if (sid == nil) return "INVALID";
              flightplan().sid = sid;
              runways = flightplan().sid.runways;
              if (size(runways) == 1) {
                var runway = departure.runways[runways[0]];
                if (runway == nil) {
                  return "INVALID RWY";
                }
                flightplan().departure_runway = runway;
              }
              elsif (size(runways) > 1) {
                var runway = flightplan().departure_runway;
                if (runway != nil and !contains(runways, runway.id)) {
                  flightplan().departure = nil;
                  flightplan().departure = departure;
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

      selectSID :: MCDU ()
      selectSID = do
        availableSIDs <- fgNasal conn [] $
          [s| var departure = flightplan().departure;
              if (departure == nil) return [];
              var runway = flightplan().departure_runway;
              if (runway == nil)
                return departure.sids()
              else
                return departure.sids(runway.id);
            |]
        case availableSIDs of
          [] ->
            scratchWarn "NO SIDS"
          sids -> do
            let handleResult Nothing = loadView departureView
                handleResult (Just sid) = do
                  setSID (Just sid) >>= \case
                    True ->
                      loadView departureView
                    False -> do
                      return ()
            loadView (selectView "SELECT SID" sids "DEPARTURE" handleResult)

  let getSID :: MCDU (Maybe ByteString)
      getSID = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.sid == nil) ? nil : fp.sid.id);
            |]

  let setTransition :: Maybe ByteString -> MCDU Bool
      setTransition Nothing = do
        fgNasal conn False $
          [s| flightplan().transition = nil;
              return 1;
            |]
      setTransition (Just transitionID) = do
        err <- fgNasal conn Nothing $
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

      selectTransition = do
        availableTransitions <- fgNasal conn [] $
          [s| var departure = flightplan().departure;
              if (departure == nil) return [];
              var sid = flightplan().sid;
              if (sid == nil)
                return [];
              else
                return sid.transitions;
            |]
        case availableTransitions of
          [] ->
            scratchWarn "NO TRANSITIONS"
          transitions -> do
            let handleResult Nothing = loadView departureView
                handleResult (Just transition) = do
                  setTransition (Just transition) >>= \case
                    True ->
                      loadView departureView
                    False -> do
                      return ()
            loadView (selectView "SELECT TRANSITION" transitions "DEPARTURE" handleResult)

  let getTransition :: MCDU (Maybe ByteString)
      getTransition = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.sid_trans == nil) ? nil : fp.sid_trans.id);
            |]

  let getDeparture :: MCDU (Maybe ByteString)
      getDeparture = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.departure == nil) ? nil : fp.departure.id);
            |]

  departureMay <- getDeparture

  case departureMay of
    Nothing ->
      fgErrorView "NO DEPARTURE"
    Just departure -> do
      runway <- getRunway
      debugPrint $ colorize cyan $ Text.pack (show runway)
      sid <- getSID
      transition <- getTransition
      sidValid <- fgNasal conn False
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
            [ (0, ("", scratchInteractOrSelect selectRunway setRunway >> reloadView))
            , (1, ("", scratchInteractOrSelect selectSID setSID >> reloadView))
            , (2, ("", scratchInteractOrSelect selectTransition setTransition >> reloadView))
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
  , mcduViewOnLoad = withFG arrivalViewLoad
  }

arrivalViewLoad :: FGFSConnection -> MCDU ()
arrivalViewLoad conn = do
  let setRunway :: Maybe ByteString -> MCDU Bool
      setRunway Nothing = do
        fgNasal conn False $
          [s| var destinationAirport = flightplan().destination;
              flightplan().destination = nil;
              flightplan().destination = destinationAirport;
              return 1;
            |]
      setRunway (Just rwyID) = do
        fgNasal conn False $
          "var runwayID = " <> encodeNasal (decodeUtf8 rwyID) <> ";\n" <>
          [s| var destination = flightplan().destination;
              if (destination == nil) return 0;
              var runway = destination.runways[runwayID];
              if (runway == nil) return 0;
              flightplan().destination_runway = runway;
              return 1;
            |]
      selectRunway = do
        runwaysMay <- fgNasal conn [] $
          [s| var runways = flightplan().destination.runways;
              return keys(runways);
            |]
        case runwaysMay of
          [] -> do
            scratchWarn "NO RUNWAYS"
          runways -> do
            let handleResult Nothing = loadView arrivalView
                handleResult (Just rwy) = do
                  setRunway (Just rwy) >>= \case
                    True ->
                      loadView arrivalView
                    False -> do
                      return ()
            loadView (selectView "SELECT RUNWAY" runways "ARRIVAL" handleResult)
          

  let getRunway :: MCDU (Maybe ByteString)
      getRunway = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              var rwyID = ((fp.destination_runway == nil) ? nil : fp.destination_runway.id);
              return rwyID
            |]

  let setSTAR :: Maybe ByteString -> MCDU Bool
      setSTAR Nothing = do
        fgNasal conn False $
          [s| flightplan().star = nil;
              return 1;
            |]
      setSTAR (Just starID) = do
        err <- fgNasal conn Nothing $
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
        availableSTARs <- fgNasal conn [] $
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

  let getSTAR :: MCDU (Maybe ByteString)
      getSTAR = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.star == nil) ? nil : fp.star.id);
            |]

  let setApproach :: Maybe ByteString -> MCDU Bool
      setApproach Nothing = do
        fgNasal conn False $
          [s| flightplan().approach = nil;
              return 1;
            |]
      setApproach (Just approachID) = do
        err <- fgNasal conn Nothing $
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
        availableApproaches <- fgNasal conn [] $
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

  let getApproach :: MCDU (Maybe ByteString)
      getApproach = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.approach == nil) ? nil : fp.approach.id);
            |]

  let setTransition :: Maybe ByteString -> MCDU Bool
      setTransition Nothing = do
        fgNasal conn False $
          [s| flightplan().transition = nil;
              return 1;
            |]
      setTransition (Just transitionID) = do
        err <- fgNasal conn Nothing $
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

      selectTransition = do
        availableTransitions <- fgNasal conn [] $
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
                  setTransition (Just transition) >>= \case
                    True ->
                      loadView arrivalView
                    False -> do
                      return ()
            loadView (selectView "SELECT TRANSITION" transitions "ARRIVAL" handleResult)

  let getTransition :: MCDU (Maybe ByteString)
      getTransition = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.star_trans == nil) ? nil : fp.star_trans.id);
            |]

  let setApproachTransition :: Maybe ByteString -> MCDU Bool
      setApproachTransition Nothing = do
        fgNasal conn False $
          [s| flightplan().approach_trans = nil;
              return 1;
            |]
      setApproachTransition (Just transitionID) = do
        err <- fgNasal conn Nothing $
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

      selectApproachTransition = do
        availableApproachTransitions <- fgNasal conn [] $
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

  let getApproachTransition :: MCDU (Maybe ByteString)
      getApproachTransition = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.approach_trans == nil) ? nil : fp.approach_trans.id);
            |]

  let getDestination :: MCDU (Maybe ByteString)
      getDestination = do
        fgNasal conn Nothing $
          [s| var fp = flightplan();
              return ((fp.destination == nil) ? nil : fp.destination.id);
            |]

  destinationMay <- getDestination

  case destinationMay of
    Nothing ->
      fgErrorView "NO ARRIVAL"
    Just destination -> do
      runway <- getRunway
      debugPrint $ colorize cyan $ Text.pack (show runway)
      star <- getSTAR
      approachTransition <- getApproachTransition
      approach <- getApproach
      transition <- getTransition
      starValid <- fgNasal conn False
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
            [ (0, ("", scratchInteractOrSelect selectRunway setRunway >> reloadView))
            , (1, ("", scratchInteractOrSelect selectApproach setApproach >> reloadView))
            , (2, ("", scratchInteractOrSelect selectSTAR setSTAR >> reloadView))
            , (6, ("", scratchInteractOrSelect selectApproachTransition setApproachTransition >> reloadView))
            , (7, ("", scratchInteractOrSelect selectTransition setTransition >> reloadView))
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

withFG :: (FGFSConnection -> MCDU ()) -> MCDU ()
withFG go = do
  connMay <- gets mcduFlightgearConnection
  case connMay of
    Nothing -> fgErrorView "NO CONNECTION"
    Just conn -> go conn `mcduCatches` handlers
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
          NasalRuntimeError msg fileMay lineMay -> do
            debugPrint $
              colorize red . Text.pack $
                "Nasal runtime error:" <> fromMaybe "?" fileMay <> ":" <> maybe "-" show lineMay <> "\n" <>
                msg
            fgErrorView "SERVER ERROR"
      , MCDUHandler $ \(e :: SomeException) -> do
            debugPrint $
              colorize red . Text.pack $
                "Error:\n" <> show e
            fgErrorView "ERROR"
      ]

