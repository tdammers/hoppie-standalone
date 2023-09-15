{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Trans
( module Web.Hoppie.Trans
, TypedMessage (..)
, TypedPayload (..)
, CPDLCMessage (..)
, ReplyOpts (..)
, CPDLCPart (..)
, WithMeta (..)
)
where

import qualified Web.Hoppie.Network as Network
import Web.Hoppie.CPDLC.Message (CPDLCMessage (..), CPDLCPart (..))
import Web.Hoppie.CPDLC.MessageTypes (ReplyOpts (..))
import Web.Hoppie.Response

import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Char8 as BS8
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar

type HoppieT = ReaderT HoppieEnv

type Hoppie = HoppieT IO

makeHoppieEnv :: MonadIO m => ByteString -> Network.Config -> m HoppieEnv
makeHoppieEnv callsign config =
  liftIO $ HoppieEnv
    config
    callsign
    <$> newMVar mempty
    <*> newMVar mempty
    <*> newMVar mempty
    <*> newMVar mempty
    <*> newMVar 0

runHoppieTWith :: HoppieEnv -> HoppieT m a -> m a
runHoppieTWith = flip runReaderT

runHoppieT :: MonadIO m => ByteString -> Network.Config -> HoppieT m a -> m a
runHoppieT callsign config action = do
  env <- makeHoppieEnv callsign config
  runHoppieTWith env action

data LinkDirection
  = Downlink
  | Uplink
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data DownlinkStatus
  = SentDownlink
  | UnsentDownlink
  | ErrorDownlink
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data UplinkStatus
  = NewUplink
  | OldUplink
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data WithMeta status a =
  WithMeta
    { metaUID :: !Word
    , metaStatus :: !status
    , metaTimestamp :: !UTCTime
    , payload :: !a
    }

data HoppieEnv =
  HoppieEnv
    { hoppieNetworkConfig :: !Network.Config
    , hoppieCallsign :: !ByteString
    , hoppieUplinks :: !(MVar (Map Word (WithMeta UplinkStatus TypedMessage)))
    , hoppieDownlinks :: !(MVar (Map Word (WithMeta DownlinkStatus TypedMessage)))
    , hoppieCPDLCUplinks :: !(MVar (Map Word Word))
    , hoppieCPDLCDownlinks :: !(MVar (Map Word Word))
    , hoppieNextUID :: !(MVar Word)
    }

saveUplink :: MonadIO m => WithMeta UplinkStatus TypedMessage -> HoppieT m ()
saveUplink tsm = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.insert (metaUID tsm) tsm
  case typedMessagePayload (payload tsm) of
    CPDLCPayload cpdlc -> do
      cpdlcUplinksVar <- asks hoppieCPDLCUplinks
      liftIO $ modifyMVar_ cpdlcUplinksVar $ return . Map.insert (cpdlcMIN cpdlc) (metaUID tsm)
    _ -> return ()

saveDownlink :: MonadIO m => WithMeta DownlinkStatus TypedMessage -> HoppieT m ()
saveDownlink tsm = do
  downlinksVar <- asks hoppieDownlinks
  liftIO $ modifyMVar_ downlinksVar $ return . Map.insert (metaUID tsm) tsm
  case typedMessagePayload (payload tsm) of
    CPDLCPayload cpdlc -> do
      cpdlcDownlinksVar <- asks hoppieCPDLCDownlinks
      liftIO $ modifyMVar_ cpdlcDownlinksVar $ return . Map.insert (cpdlcMIN cpdlc) (metaUID tsm)
    _ -> return ()

setUplinkStatus :: MonadIO m => Word -> UplinkStatus -> HoppieT m ()
setUplinkStatus uid status = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.adjust (\uplink -> uplink { metaStatus = status }) uid

setDownlinkStatus :: MonadIO m => Word -> DownlinkStatus -> HoppieT m ()
setDownlinkStatus uid status = do
  downlinksVar <- asks hoppieDownlinks
  liftIO $ modifyMVar_ downlinksVar $ return . Map.adjust (\downlink -> downlink { metaStatus = status }) uid

getUplink :: MonadIO m => Word -> HoppieT m (Maybe (WithMeta UplinkStatus TypedMessage))
getUplink uid = do
  uplinkVar <- asks hoppieUplinks
  Map.lookup uid <$> liftIO (readMVar uplinkVar)

getDownlink :: MonadIO m => Word -> HoppieT m (Maybe (WithMeta DownlinkStatus TypedMessage))
getDownlink uid = do
  downlinkVar <- asks hoppieDownlinks
  Map.lookup uid <$> liftIO (readMVar downlinkVar)

getCPDLCUplink :: MonadIO m => Word -> HoppieT m (Maybe (WithMeta UplinkStatus TypedMessage))
getCPDLCUplink cMIN = do
  cpdlcUplinkVar <- asks hoppieCPDLCUplinks
  uidMay <- Map.lookup cMIN <$> liftIO (readMVar cpdlcUplinkVar)
  case uidMay of
    Nothing -> return Nothing
    Just uid -> getUplink uid

getCPDLCDownlink :: MonadIO m => Word -> HoppieT m (Maybe (WithMeta DownlinkStatus TypedMessage))
getCPDLCDownlink cMIN = do
  cpdlcDownlinkVar <- asks hoppieCPDLCDownlinks
  uidMay <- Map.lookup cMIN <$> liftIO (readMVar cpdlcDownlinkVar)
  case uidMay of
    Nothing -> return Nothing
    Just uid -> getDownlink uid

makeUID :: MonadIO m => HoppieT m Word
makeUID = do
  uidVar <- asks hoppieNextUID
  liftIO $ modifyMVar uidVar $ \uid -> do
    return (succ uid, uid)

send :: MonadIO m => TypedMessage -> HoppieT m [Word]
send tm = do
  ts <- liftIO getCurrentTime
  uid <- makeUID
  let rq = WithMeta uid UnsentDownlink ts tm
  config <- asks hoppieNetworkConfig
  sender <- asks hoppieCallsign
  saveDownlink rq
  rawResponse <- liftIO $ Network.sendRequest config (toUntypedRequest sender tm)
  processResponse (Just uid) rawResponse

processResponse :: MonadIO m => Maybe Word -> ByteString -> HoppieT m [Word]
processResponse uidMay rawResponse = do
  ts <- liftIO getCurrentTime
  case parseResponse rawResponse of
    Left err ->
      error err
    Right (ErrorResponse err) ->
      error (BS8.unpack err)
    Right (Response []) -> do
      return []
    Right (Response messages) -> do
      forM messages $ \msg -> do
        uid <- makeUID
        maybe
          (return ())
          (\parentUID -> setDownlinkStatus parentUID SentDownlink)
          uidMay
        saveUplink (WithMeta uid NewUplink ts $ toTypedMessage msg)
        return uid

poll :: MonadIO m => HoppieT m [Word]
poll = do
  config <- asks hoppieNetworkConfig
  receiver <- asks hoppieCallsign
  rawResponse <- liftIO $ Network.sendRequest config
                    Network.Request
                      { Network.requestFrom = receiver
                      , Network.requestTo = "SERVER"
                      , Network.requestType = Network.Poll
                      , Network.requestPacket = ""
                      }
  processResponse Nothing rawResponse
