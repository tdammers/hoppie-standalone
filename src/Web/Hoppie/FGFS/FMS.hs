{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.FGFS.FMS where

import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.FGFS.Monad

import Data.Text (Text)
import qualified Data.Text as Text
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
import Data.Word

lbs2kg :: Double
lbs2kg = 0.45359237

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
    , legEFOB :: Maybe Double
    , legETE :: Maybe Double
    , legIdxFrom :: Int
    , legIdxTo :: Int
    }
    deriving (Show)

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
      <*> fromNasalFieldMaybe "efob" n
      <*> fromNasalFieldMaybe "ete" n
      <*> fromNasalField "ifrom" n
      <*> fromNasalField "ito" n

data RouteLeg =
  RouteLeg
    { routeLegVia :: Maybe ByteString
    , routeLegTo :: ByteString
    , routeLegDistance :: Maybe Double
    , routeLegFromIndex :: Int
    , routeLegToIndex :: Int
    , routeLegArrDep :: Maybe Text
    }
    deriving (Show)

instance FromNasal RouteLeg where
  fromNasal n =
    RouteLeg
      <$> fromNasalFieldMaybe "via" n
      <*> fromNasalField "to" n
      <*> fromNasalFieldMaybe "dist" n
      <*> fromNasalField "fromIndex" n
      <*> fromNasalField "toIndex" n
      <*> fromNasalFieldMaybe "is" n

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

instance FromNasal WaypointCandidate where
  fromNasal nv =
    WaypointCandidate
      <$> fromNasalField "id" nv
      <*> fromNasalField "type" nv
      <*> fromNasalField "name" nv
      <*> fromNasalField "distance" nv
      <*> fromNasalField "bearing" nv
      <*> fromNasalField "wp" nv

data FlightPhase
      = ON_STAND
      | TAXI_OUT
      | TAKEOFF
      | CLIMB
      | CRUISE
      | DESCENT
      | APPROACH
      | TAXI_IN
      | GO_AROUND
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

nextFlightPhase :: FlightPhase -> FlightPhase
nextFlightPhase GO_AROUND = CLIMB
nextFlightPhase TAXI_IN = ON_STAND
nextFlightPhase x = succ x

prevFlightPhase :: FlightPhase -> Maybe FlightPhase
prevFlightPhase ON_STAND = Nothing
prevFlightPhase GO_AROUND = Just APPROACH
prevFlightPhase x = Just (pred x)

instance FromNasal FlightPhase where
  fromNasal nv =
    toEnum <$> fromNasal nv

instance ToNasal FlightPhase where
  toNasal = toNasal . fromEnum

data RnpInfo =
  RnpInfo
    { rnpRNP :: Maybe Double
    , rnpANP :: Maybe Double
    , rnpSensorName :: Maybe ByteString
    }
    deriving Show

instance FromNasal RnpInfo where
  fromNasal nv =
    RnpInfo
      <$> fromNasalFieldMaybe "rnp" nv
      <*> fromNasalFieldMaybe "anp" nv
      <*> fromNasalFieldMaybe "sensor" nv

data ProgressInfo =
  ProgressInfo
    { progressCurrent :: Maybe FPLeg
    , progressNext :: Maybe FPLeg
    , progressDestination :: Maybe FPLeg
    , progressFOB :: Maybe Double
    , progressGrossWeight :: Maybe Double
    , progressFlightPhase :: FlightPhase
    , progressDistToTOC :: Maybe FPLeg
    , progressDistToTOD :: Maybe FPLeg
    , progressRNP :: Maybe RnpInfo
    , progressAlerts :: [ByteString]
    }
    deriving (Show)

instance FromNasal ProgressInfo where
  fromNasal nv =
    ProgressInfo
      <$> fromNasalFieldMaybe "current" nv
      <*> fromNasalFieldMaybe "next" nv
      <*> fromNasalFieldMaybe "destination" nv
      <*> fromNasalFieldMaybe "fob" nv
      <*> fromNasalFieldMaybe "gw" nv
      <*> fromNasalField "phase" nv
      <*> fromNasalFieldMaybe "toc" nv
      <*> fromNasalFieldMaybe "tod" nv
      <*> fromNasalFieldMaybe "rnp" nv
      <*> (fromMaybe [] <$> fromNasalFieldMaybe "alerts" nv)

data IASOrMach
  = IAS Int
  | Mach Double
  deriving (Show, Eq)

instance ToNasal IASOrMach where
  toNasal (IAS i) = toNasal i
  toNasal (Mach m) = toNasal . Text.pack $ printf "M%02.0f" (m * 100)

instance FromNasal IASOrMach where
  fromNasal (NasalInt i) = pure $ IAS i
  fromNasal (NasalFloat f) = pure $ IAS (round f)
  fromNasal n@(NasalString t)
    | Text.take 1 t == "M"
    = case readMaybe (Text.unpack . Text.drop 1 $ t) of
        Nothing -> Left $ NasalUnexpected "IAS/Mach" (show n)
        Just m -> pure $ Mach $ m / 100
  fromNasal n = Left $ NasalUnexpected "IAS/Mach" (show n)

data ClimbPerfOf a =
  ClimbPerf
    { climbSpeed :: IASOrMach
    , climbRate :: a
    , climbToFL :: Int
    }
    deriving (Show, Eq)

type ClimbPerf = ClimbPerfOf Int

instance ToNasal a => ToNasal (ClimbPerfOf a) where
  toNasal c =
    NasalHash $ Map.fromList
      [ ("spd", toNasal (climbSpeed c))
      , ("roc", toNasal (climbRate c))
      , ("fl", toNasal (climbToFL c))
      ]

instance FromNasal a => FromNasal (ClimbPerfOf a) where
  fromNasal n =
    ClimbPerf
      <$> fromNasalField "spd" n
      <*> fromNasalField "roc" n
      <*> fromNasalField "fl" n

data PerfInitData =
  PerfInitData
    { perfInitZFW :: Maybe Double
    , perfInitBlockFuel :: Maybe Double
    , perfInitMinTakeoffFuel :: Maybe Double
    , perfInitReserveFuel :: Maybe Double
    , perfInitContingencyFuel :: Maybe Double
    , perfInitClimbProfile :: [ClimbPerf]
    , perfInitCruiseAlt :: Maybe Int
    , perfInitCruiseFL :: Maybe Int
    , perfInitCruiseIAS :: Maybe Int
    , perfInitCruiseMach :: Maybe Double
    , perfInitCruiseWind :: Maybe (Int, Int)
    , perfInitDescentFPA :: Maybe Double
    , perfInitDescentMach :: Maybe Double
    , perfInitDescentIAS :: Maybe Int
    , perfInitTransAlt :: Maybe Int
    , perfInitTransFL :: Maybe Int
    }
    deriving (Show, Eq)

defPerfInitData :: PerfInitData
defPerfInitData =
  PerfInitData
    { perfInitZFW = Nothing
    , perfInitBlockFuel = Nothing
    , perfInitMinTakeoffFuel = Nothing
    , perfInitReserveFuel = Nothing
    , perfInitContingencyFuel = Nothing
    , perfInitClimbProfile = []
    , perfInitCruiseAlt = Nothing
    , perfInitCruiseFL = Nothing
    , perfInitCruiseIAS = Nothing
    , perfInitCruiseMach = Nothing
    , perfInitCruiseWind = Nothing
    , perfInitDescentFPA = Nothing
    , perfInitDescentMach = Nothing
    , perfInitDescentIAS = Nothing
    , perfInitTransAlt = Nothing
    , perfInitTransFL = Nothing
    }

instance ToNasal PerfInitData where
  toNasal pd =
    NasalHash $ Map.fromList $
      [ ("zfw", toNasal (perfInitZFW pd))
      , ("blockFuel", toNasal (perfInitBlockFuel pd))
      , ("toFuel", toNasal (perfInitMinTakeoffFuel pd))
      , ("reserveFuel", toNasal (perfInitReserveFuel pd))
      , ("contFuel", toNasal (perfInitContingencyFuel pd))
      , ("crzWind", toNasal (perfInitCruiseWind pd))
      , ("desFPA", toNasal (perfInitDescentFPA pd))
      , ("desMach", toNasal (perfInitDescentMach pd))
      , ("desIAS", toNasal (perfInitDescentIAS pd))
      , ("transAlt", toNasal (perfInitTransAlt pd))
      , ("transFL", toNasal (perfInitTransFL pd))
      ]
      ++
      [ ("climb", toNasal (perfInitClimbProfile pd))
      | not . null $ perfInitClimbProfile pd
      ]
      ++
      [ ("crzAlt", toNasal (perfInitCruiseAlt pd))
      | isJust (perfInitCruiseAlt pd) || isNothing (perfInitCruiseFL pd)
      ]
      ++
      [ ("crzFL", toNasal (perfInitCruiseFL pd))
      | isJust (perfInitCruiseFL pd)
      ] ++
      [ ("crzIAS", toNasal (perfInitCruiseIAS pd))
      | isJust (perfInitCruiseIAS pd) || isNothing (perfInitCruiseMach pd)
      ]
      ++
      [ ("crzMach", toNasal (perfInitCruiseMach pd))
      | isJust (perfInitCruiseMach pd)
      ]

instance FromNasal PerfInitData where
  fromNasal nv =
    PerfInitData
      <$> fromNasalFieldMaybe "zfw" nv
      <*> fromNasalFieldMaybe "blockFuel" nv
      <*> fromNasalFieldMaybe "toFuel" nv
      <*> fromNasalFieldMaybe "reserveFuel" nv
      <*> fromNasalFieldMaybe "contFuel" nv
      <*> (fromMaybe [] <$> fromNasalFieldMaybe "climb" nv)
      <*> (nothingIfJustZero <$> fromNasalFieldMaybe "crzAlt" nv)
      <*> (nothingIfJustZero <$> fromNasalFieldMaybe "crzFL" nv)
      <*> (nothingIfJustZero <$> fromNasalFieldMaybe "crzIAS" nv)
      <*> (nothingIfJustZero <$> fromNasalFieldMaybe "crzMach" nv)
      <*> fromNasalFieldMaybe "crzWind" nv
      <*> fromNasalFieldMaybe "desFPA" nv
      <*> fromNasalFieldMaybe "desMach" nv
      <*> fromNasalFieldMaybe "desIAS" nv
      <*> fromNasalFieldMaybe "transAlt" nv
      <*> fromNasalFieldMaybe "transFL" nv
    where
      nothingIfJustZero (Just 0) = Nothing
      nothingIfJustZero x = x

releaseWaypointCandidate :: (MonadFG m) => WaypointCandidate -> m ()
releaseWaypointCandidate candidate =
  fgCallNasal "release" [wpValue candidate]

acquireWaypointCandidate :: (MonadFG m) => WaypointCandidate -> m ()
acquireWaypointCandidate candidate =
  fgCallNasal "acquire" [wpValue candidate]

getFGCallsign :: (MonadFG m) => m (Maybe ByteString)
getFGCallsign = fgCallNasal "fms.getFGCallsign" ()

setFGCallsign :: (MonadFG m) => ByteString -> m ()
setFGCallsign cs = fgCallNasal "fms.setFGCallsign" [cs]

getFGAircraftType :: (MonadFG m) => m (Maybe ByteString)
getFGAircraftType = fgCallNasal "fms.getAircraftType" ()

insertDirect :: forall m. (MonadFG m) => Maybe WaypointCandidate -> WaypointCandidate -> m (Either Text ())
insertDirect fromWPMay toWP = do
  let nasalFunc = case wpType toWP of
        "leg" -> "fms.insertDirectFP"
        _ -> "fms.insertDirect"
  result <- (fgCallNasal nasalFunc (wpValue toWP, wpValue <$> fromWPMay) :: m (Maybe Text))
  case result of
    Just err -> return $ Left err
    Nothing -> return $ Right ()

getRoute :: (MonadFG m) => m [Maybe RouteLeg]
getRoute =
  fgCallNasal "fms.getRoute" ()

appendViaTo :: (MonadFG m) => ByteString -> ByteString -> m (Maybe ByteString)
appendViaTo via to =
  fgCallNasal "fms.appendViaTo" (via, to)

appendDirectTo :: (MonadFG m) => WaypointCandidate -> m (Maybe ByteString)
appendDirectTo toWP =
  fgCallNasal "fms.appendDirectTo" [wpValue toWP]

deleteRouteLeg :: (MonadFG m) => Int -> Int -> m ()
deleteRouteLeg fromIndex toIndex =
  fgCallNasal "fms.deleteRouteLeg" [fromIndex, toIndex]

resolveLeg :: (MonadFG m) => ByteString -> (Maybe WaypointCandidate -> m ()) -> m ()
resolveLeg name cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getFPLegIndex" [name]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

getFlightplanLeg :: (MonadFG m) => Int -> (Maybe WaypointCandidate -> m ()) -> m ()
getFlightplanLeg index cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getWaypoint" [index]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

resolveWaypoint :: forall m.
                   (MonadFG m)
                => Bool
                -> ByteString
                -> (ByteString -> m ())
                -> ([WaypointCandidate] -> (Maybe WaypointCandidate -> m ()) -> m ())
                -> (Maybe WaypointCandidate -> m ())
                -> m ()
resolveWaypoint includeLegs name warn sel cont = do
  candidates :: [WaypointCandidate] <- fgCallNasal "fms.findWaypoint" (includeLegs, name)
  case candidates of
    [] -> do
      warn "NO WPT"
      cont Nothing
    [candidate] -> do
      cont (Just candidate)
    _ -> do
      mapM_ acquireWaypointCandidate candidates
      sel candidates cont

findFPWaypoint :: (MonadFG m) => WaypointCandidate -> m (Maybe Int)
findFPWaypoint wp = fgCallNasalDef Nothing "fms.findFPWaypoint" ((), Just $ wpValue wp)

getWaypointName :: (MonadFG m) => Int -> m (Maybe ByteString)
getWaypointName n = fgCallNasalDef Nothing "fms.getWaypointName" [n]

setFPLegSpeed :: (MonadFG m) => Int -> Maybe Int -> ByteString -> m (Maybe ByteString)
setFPLegSpeed n spd restr = fgCallNasalDef Nothing "fms.setLegSpeed" (n, spd, restr)

setFPLegAltitude :: (MonadFG m) => Int -> Maybe Int -> ByteString -> m (Maybe ByteString)
setFPLegAltitude n spd restr = fgCallNasalDef Nothing "fms.setLegAltitude" (n, spd, restr)

clearFlightplan :: (MonadFG m) => m Bool
clearFlightplan = fgCallNasalBool "fms.clearFlightplan" ()

listDepartureRunways :: (MonadFG m) => m [ByteString]
listDepartureRunways = fgCallNasal "fms.listDepartureRunways" ()

listDestinationRunways :: (MonadFG m) => m [ByteString]
listDestinationRunways = fgCallNasal "fms.listDestinationRunways" ()

listSIDs :: (MonadFG m) => m [ByteString]
listSIDs = fgCallNasal "fms.listSIDs" ()

listSidTransitions :: (MonadFG m) => m [ByteString]
listSidTransitions = fgCallNasal "fms.listSidTransitions" ()

listSTARs :: (MonadFG m) => m [ByteString]
listSTARs = fgCallNasal "fms.listSTARs" ()

listStarTransitions :: (MonadFG m) => m [ByteString]
listStarTransitions = fgCallNasal "fms.listStarTransitions" ()

listApproaches :: (MonadFG m) => m [ByteString]
listApproaches = fgCallNasal "fms.listApproaches" ()

listApproachTransitions :: (MonadFG m) => m [ByteString]
listApproachTransitions = fgCallNasal "fms.listApproachTransitions" ()

setDeparture :: (MonadFG m) =>  Maybe ByteString -> m Bool
setDeparture icao = do
  fgCallNasalBool "fms.setDeparture" [icao]

getDeparture :: (MonadFG m) =>  m (Maybe ByteString)
getDeparture = fgCallNasal "fms.getDeparture" ()

setDepartureRunway :: (MonadFG m) =>  Maybe ByteString -> m Bool
setDepartureRunway rwyID =
  fgCallNasalBool "fms.setDepartureRunway" [rwyID]

getDepartureRunway :: (MonadFG m) =>  m (Maybe ByteString)
getDepartureRunway =
  fgCallNasal "fms.getDepartureRunway" ()

setSID :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setSID sidID = do
  fgCallNasal "fms.setSID" [sidID]

getSID :: (MonadFG m) =>  m (Maybe ByteString)
getSID = fgCallNasal "fms.getSID" ()


setSidTransition :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setSidTransition sidID = do
  fgCallNasal "fms.setSidTransition" [sidID]

getSidTransition :: (MonadFG m) =>  m (Maybe ByteString)
getSidTransition = fgCallNasal "fms.getSidTransition" ()

setDestination :: (MonadFG m) =>  Maybe ByteString -> m Bool
setDestination icao = fgCallNasalBool "fms.setDestination" [icao]

getDestination :: (MonadFG m) =>  m (Maybe ByteString)
getDestination = fgCallNasal "fms.getDestination" ()


getDestinationRunway :: (MonadFG m) =>  m (Maybe ByteString)
getDestinationRunway =
  fgCallNasal "fms.getDestinationRunway" ()

setDestinationRunway :: (MonadFG m) =>  Maybe ByteString -> m Bool
setDestinationRunway rwyID =
  fgCallNasalBool "fms.setDestinationRunway" [rwyID]

setSTAR :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setSTAR starID = do
  fgCallNasal "fms.setSTAR" [starID]

getSTAR :: (MonadFG m) =>  m (Maybe ByteString)
getSTAR = fgCallNasal "fms.getSTAR" ()


setStarTransition :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setStarTransition starID = do
  fgCallNasal "fms.setStarTransition" [starID]

getStarTransition :: (MonadFG m) =>  m (Maybe ByteString)
getStarTransition = fgCallNasal "fms.getStarTransition" ()


setApproach :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setApproach approachID = do
  fgCallNasal "fms.setApproach" [approachID]


getApproach :: (MonadFG m) =>  m (Maybe ByteString)
getApproach = fgCallNasal "fms.getApproach" ()


setApproachTransition :: (MonadFG m) =>  Maybe ByteString -> m (Maybe ByteString)
setApproachTransition approachID = do
  fgCallNasal "fms.setApproachTransition" [approachID]

getApproachTransition :: (MonadFG m) => m (Maybe ByteString)
getApproachTransition = fgCallNasal "fms.getApproachTransition" ()


isValidSID :: (MonadFG m) => m Bool
isValidSID = fgCallNasalBool "fms.isValidSID" ()

isValidSTAR :: (MonadFG m) => m Bool
isValidSTAR = fgCallNasalBool "fms.isValidSTAR" ()





cancelFlightplanEdits :: (MonadFG m) => m ()
cancelFlightplanEdits = fgCallNasal "fms.cancelFlightplanEdits" ()

commitFlightplanEdits :: (MonadFG m) => m ()
commitFlightplanEdits = fgCallNasal "fms.commitFlightplanEdits" ()

getGroundspeed :: (MonadFG m) => m Double
getGroundspeed = fgCallNasalDef 0 "fms.getGroundspeed" ()

getUTCMinutes :: (MonadFG m) => m Double
getUTCMinutes = fgCallNasalDef 0 "fms.getUTCMinutes" ()

getFlightplanSize :: (MonadFG m) => m Int
getFlightplanSize = fgCallNasalDef 0 "fms.getFlightplanSize" ()

getCurrentLeg :: (MonadFG m) => m (Maybe Int)
getCurrentLeg = fgCallNasalDef Nothing "fms.getCurrentLeg" ()

getFlightplanLegs :: (MonadFG m) => Int -> Int -> Int -> m [FPLeg]
getFlightplanLegs legsPerPage curPage currentLeg =
  fgCallNasal "fms.getFlightplanLegs" (legsPerPage, curPage, currentLeg)

getProgressInfo :: (MonadFG m) => m (Maybe ProgressInfo)
getProgressInfo =
  fgCallNasalDef Nothing "fms.getProgressInfo" ()

hasFlightplanModifications :: (MonadFG m) => m Bool
hasFlightplanModifications =
  fgCallNasalBool "fms.hasFlightplanModifications" ()

deleteWaypoint :: (MonadFG m) => Int -> m Bool
deleteWaypoint n = fgCallNasalBool "fms.deleteWaypoint" [n]

getTransitionAlt :: (MonadFG m) => m (Maybe Double)
getTransitionAlt = fgCallNasalDef Nothing "fms.getTransitionAlt" ()

getPerfInitData :: (MonadFG m) => m PerfInitData
getPerfInitData = fgCallNasalDef defPerfInitData "fms.getPerfInitData" ()

setPerfInitData :: (MonadFG m) => PerfInitData -> m ()
setPerfInitData pd = fgCallNasal "fms.setPerfInitData" [pd]

getFuelOnBoard :: (MonadFG m) => m (Maybe Double)
getFuelOnBoard = fgCallNasalDef Nothing "fms.getFuelOnBoard" ()

getFuelCapacity :: (MonadFG m) => m Double
getFuelCapacity = fgCallNasalDef 0 "fms.getFuelCapacity" ()

getFlightPhase :: (MonadFG m) => m (Maybe FlightPhase)
getFlightPhase = fgCallNasalDef Nothing "fms.getFlightPhase" ()

setFlightPhase :: (MonadFG m) => FlightPhase -> m ()
setFlightPhase p = fgCallNasal "fms.setFlightPhase" [p]
