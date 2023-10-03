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

data ProgressInfo =
  ProgressInfo
    { progressCurrent :: Maybe FPLeg
    , progressNext :: Maybe FPLeg
    , progressDestination :: Maybe FPLeg
    , progressFOB :: Maybe Double
    }
    deriving (Show)

instance FromNasal ProgressInfo where
  fromNasal nv =
    ProgressInfo
      <$> fromNasalFieldMaybe "current" nv
      <*> fromNasalFieldMaybe "next" nv
      <*> fromNasalFieldMaybe "destination" nv
      <*> fromNasalFieldMaybe "fob" nv

data PerfInitData =
  PerfInitData
    { perfInitZFW :: Maybe Double
    , perfInitBlockFuel :: Maybe Double
    , perfInitMinTakeoffFuel :: Maybe Double
    , perfInitReserveFuel :: Maybe Double
    , perfInitContingencyFuel :: Maybe Double
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
    }

instance ToNasal PerfInitData where
  toNasal pd =
    NasalHash $ Map.fromList
      [ ("zfw", toNasal (perfInitZFW pd))
      , ("blockFuel", toNasal (perfInitBlockFuel pd))
      , ("toFuel", toNasal (perfInitMinTakeoffFuel pd))
      , ("reserveFuel", toNasal (perfInitReserveFuel pd))
      , ("contFuel", toNasal (perfInitContingencyFuel pd))
      ]

instance FromNasal PerfInitData where
  fromNasal nv =
    PerfInitData
      <$> fromNasalFieldMaybe "zfw" nv
      <*> fromNasalFieldMaybe "blockFuel" nv
      <*> fromNasalFieldMaybe "toFuel" nv
      <*> fromNasalFieldMaybe "reserveFuel" nv
      <*> fromNasalFieldMaybe "contFuel" nv

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

insertDirect :: forall m. (MonadFG m) => Maybe WaypointCandidate -> WaypointCandidate -> m (Either Text ())
insertDirect fromWPMay toWP = do
  let nasalFunc = case wpType toWP of
        "leg" -> "fms.insertDirectFP"
        _ -> "fms.insertDirect"
  result <- (fgCallNasal nasalFunc (wpValue toWP, wpValue <$> fromWPMay) :: m (Maybe Text))
  case result of
    Just err -> return $ Left err
    Nothing -> return $ Right ()

resolveLeg :: (MonadFG m) => ByteString -> (Maybe WaypointCandidate -> m ()) -> m ()
resolveLeg name cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getFPLegIndex" [name]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

getLeg :: (MonadFG m) => Int -> (Maybe WaypointCandidate -> m ()) -> m ()
getLeg index cont = do
  candidate :: Maybe WaypointCandidate <- fgCallNasalDef Nothing "fms.getWaypoint" [index]
  cont candidate
  mapM_ releaseWaypointCandidate candidate

resolveWaypoint :: forall m.
                   (MonadFG m)
                => ByteString
                -> (ByteString -> m ())
                -> ([WaypointCandidate] -> (Maybe WaypointCandidate -> m ()) -> m ())
                -> (Maybe WaypointCandidate -> m ())
                -> m ()
resolveWaypoint name warn sel cont = do
  candidates :: [WaypointCandidate] <- fgCallNasal "fms.findWaypoint" [name]
  case candidates of
    [] -> do
      warn "NO WPT"
      cont Nothing
    [candidate] -> do
      cont (Just candidate)
      releaseWaypointCandidate candidate
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

clearFlightplan :: (MonadFG m) => m ()
clearFlightplan = fgCallNasal "fms.clearFlightplan" ()

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

getPerfInitData :: (MonadFG m) => m PerfInitData
getPerfInitData = fgCallNasalDef defPerfInitData "fms.getPerfInitData" ()

setPerfInitData :: (MonadFG m) => PerfInitData -> m ()
setPerfInitData pd = fgCallNasal "fms.setPerfInitData" [pd]

getFuelOnBoard :: (MonadFG m) => m (Maybe Double)
getFuelOnBoard = fgCallNasalDef Nothing "fms.getFuelOnBoard" ()
