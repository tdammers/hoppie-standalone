{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Common
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
      <*> fromNasalField "heading" n
      <*> fromNasalField "leg_dist" n
      <*> fromNasalField "route_dist" n
      <*> fromNasalField "remaining_dist" n
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

navView :: MCDUView
navView = defView
  { mcduViewTitle = "NAV MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("DIRECT", scratchWarn "NOT IMPLEMENTED"))
      , (LSKL 1, ("NAV INIT", scratchWarn "NOT IMPLEMENTED"))
      , (LSKL 5, ("MENU", loadViewByID MainMenuView))
      ]
  }

fplView :: MCDUView
fplView = defView
  { mcduViewTitle = "ACT FPL"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = fplViewLoad
  }

fplViewLoad :: MCDU ()
fplViewLoad = withFGView $ \conn -> do
  (groundspeed :: Double) <- max 100 <$> callNasalFunc conn "fms.getGroundspeed" ()
  (utcMinutes :: Double) <- callNasalFunc conn "fms.getUTCMinutes" ()
  legs' <- callNasalFunc conn "fms.getFlightplanLegs" ()
  currentLeg <- callNasalFunc conn "fms.getCurrentLeg" ()
  flightplanModified <- callNasalFunc conn "fms.hasFlightplanModifications" ()
  let legsPerPage = numLSKs
  let legs = case currentLeg of
                Nothing -> legs'
                Just i -> drop (i - 1) legs'
  curPage <- gets (mcduViewPage . mcduView)
  let legsDropped = curPage * legsPerPage
  let (numPages, curLegs) = paginate legsPerPage curPage legs
  modifyView $ \v -> v
    { mcduViewNumPages = numPages
    , mcduViewTitle = if flightplanModified then "MOD FPL" else "ACT FPL"
    , mcduViewDraw = do
        when (null legs) $ do
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
            forM_ (legRemainingDist leg) $ \dist -> do
              when (groundspeed > 40) $ do
                let eta = utcMinutes + dist / groundspeed * 60
                mcduPrint (screenW - 6) (n * 2 + 1) color (BS8.pack $ formatETA eta <> "z")
            if isPrevious then
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            else if isCurrent then do
              mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
              mcduPrint 9 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legRemainingDist leg)
              mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
            else do
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
        , (LSKL 4, ("DEPARTURE", loadView departureView))

        , (LSKR 0, ("", do
                    scratchInteract
                      setDestination
                      getDestination
                    reloadView))
        , (LSKR 1, ("", do
                    scratchInteract
                      (maybe (return False) (\c -> lift (setCallsign c) >> return True))
                      (Just <$> lift getCallsign)
                    reloadView))
        , (LSKR 4, ("ARRIVAL", loadView arrivalView))
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
          [ (n, (option, handleResult (Just option)))
          | (n, option) <- zip [LSKL 0 .. LSKR 4] curOptions
          ]
      }
      addLskBinding (LSKL 5) returnLabel (handleResult Nothing)
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

warnOrSucceed :: Maybe ByteString -> MCDU Bool
warnOrSucceed Nothing = return True
warnOrSucceed (Just e) = do
  scratchWarn e
  return False

setDeparture :: Maybe ByteString -> MCDU Bool
setDeparture icao = fgCallNasalBool "fms.setDeparture" [icao]

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
