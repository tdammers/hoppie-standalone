{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Trans
( module Web.Hoppie.Trans
, TypedMessage (..)
, TypedPayload (..)
, CPDLCMessage (..)
, ReplyOpts (..)
, CPDLCPart (..)
, WithMeta (..)
, Network.defURL
, Network.Config (..)
)
where

import qualified Web.Hoppie.Network as Network
import Web.Hoppie.CPDLC.Message (CPDLCMessage (..), CPDLCPart (..))
import Web.Hoppie.CPDLC.MessageTypes (ReplyOpts (..))
import Web.Hoppie.Response

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar
import Data.List

type HoppieT = ReaderT HoppieEnv

type Hoppie = HoppieT IO

data HoppieEnv =
  HoppieEnv
    { hoppieNetworkConfig :: !Network.Config
    , hoppieCallsign :: !ByteString
    , hoppieUplinks :: !(MVar (Map Word (WithMeta UplinkStatus TypedMessage)))
    , hoppieDownlinks :: !(MVar (Map Word (WithMeta DownlinkStatus TypedMessage)))
    , hoppieCPDLCUplinks :: !(MVar (Map Word Word))
    , hoppieCPDLCDownlinks :: !(MVar (Map Word Word))
    , hoppieNextUID :: !(MVar Word)
    , hoppieNetworkStatus :: !(MVar NetworkStatus)
    , hoppieFastPollingCounter :: !(MVar Int)
    }

data NetworkStatus
  = NetworkOK
  | NetworkError String
  deriving (Show, Eq)

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
    <*> newMVar NetworkOK
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
    deriving (Show)

data HoppieMessage
  = UplinkMessage (WithMeta UplinkStatus TypedMessage)
  | DownlinkMessage (WithMeta DownlinkStatus TypedMessage)

messageUID :: HoppieMessage -> Word
messageUID (UplinkMessage m) = metaUID m
messageUID (DownlinkMessage m) = metaUID m

messageTimestamp :: HoppieMessage -> UTCTime
messageTimestamp (UplinkMessage m) = metaTimestamp m
messageTimestamp (DownlinkMessage m) = metaTimestamp m

messageFrom :: ByteString -> HoppieMessage -> ByteString
messageFrom myCallsign (DownlinkMessage _) = myCallsign
messageFrom _ (UplinkMessage m) = typedMessageCallsign . payload $ m

messageTo :: ByteString -> HoppieMessage -> ByteString
messageTo myCallsign (UplinkMessage _) = myCallsign
messageTo _ (DownlinkMessage m) = typedMessageCallsign . payload $ m

messagePayload :: HoppieMessage -> TypedMessage
messagePayload (UplinkMessage m) = payload m
messagePayload (DownlinkMessage m) = payload m

setNetworkStatus :: MonadIO m => NetworkStatus -> HoppieT m ()
setNetworkStatus s = do
  statusVar <- asks hoppieNetworkStatus
  liftIO $ modifyMVar_ statusVar (const . return $ s)

getAllMessages :: MonadIO m => HoppieT m [HoppieMessage]
getAllMessages = do
  uplinks <- fmap (map UplinkMessage . Map.elems) $
              asks hoppieUplinks >>= liftIO . readMVar
  downlinks <- fmap (map DownlinkMessage . Map.elems) $
              asks hoppieDownlinks >>= liftIO . readMVar
  return $ sortOn messageUID $ uplinks ++ downlinks

saveUplink :: MonadIO m => WithMeta UplinkStatus TypedMessage -> HoppieT m ()
saveUplink tsm = do
  case typedMessagePayload (payload tsm) of
    CPDLCPayload cpdlc -> do
      cpdlcUplinksVar <- asks hoppieCPDLCUplinks
      liftIO $ modifyMVar_ cpdlcUplinksVar $ return . Map.insert (cpdlcMIN cpdlc) (metaUID tsm)
    _ -> do
      uplinksVar <- asks hoppieUplinks
      liftIO $ modifyMVar_ uplinksVar $ return . Map.insert (metaUID tsm) tsm

saveDownlink :: MonadIO m => WithMeta DownlinkStatus TypedMessage -> HoppieT m ()
saveDownlink tsm = do
  case typedMessagePayload (payload tsm) of
    CPDLCPayload cpdlc -> do
      cpdlcDownlinksVar <- asks hoppieCPDLCDownlinks
      liftIO $ modifyMVar_ cpdlcDownlinksVar $ return . Map.insert (cpdlcMIN cpdlc) (metaUID tsm)
    _ -> do
      downlinksVar <- asks hoppieDownlinks
      liftIO $ modifyMVar_ downlinksVar $ return . Map.insert (metaUID tsm) tsm

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

getMessage :: MonadIO m => Word -> HoppieT m (Maybe HoppieMessage)
getMessage uid = do
  dl <- getDownlink uid
  ul <- getUplink uid
  return $ (UplinkMessage <$> ul) <|> (DownlinkMessage <$> dl)

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

getCPDLCMessage :: MonadIO m => Word -> HoppieT m (Maybe HoppieMessage)
getCPDLCMessage uid = do
  dl <- getCPDLCDownlink uid
  ul <- getCPDLCUplink uid
  return $ (UplinkMessage <$> ul) <|> (DownlinkMessage <$> dl)

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
  rawResponse <- liftIO $ Network.sendRequestEither config (toUntypedRequest sender tm)
  case rawResponse of
    Left err -> do
      setDownlinkStatus uid ErrorDownlink
      setNetworkStatus $ NetworkError err
      return []
    Right response -> do
      setDownlinkStatus uid SentDownlink
      processResponse (Just uid) response

makeErrorResponse :: MonadIO m => Maybe MessageType -> ByteString -> String -> HoppieT m [Word]
makeErrorResponse tyM body err = do
  uid <- makeUID
  ts <- liftIO getCurrentTime
  saveUplink $
    WithMeta uid NewUplink ts
      (TypedMessage (Just uid) "SYSTEM" (ErrorPayload tyM body err))
  return [uid]

processResponse :: MonadIO m => Maybe Word -> ByteString -> HoppieT m [Word]
processResponse uidMay rawResponse = do
  ts <- liftIO getCurrentTime
  case parseResponse rawResponse of
    Left err ->
      makeErrorResponse Nothing "PARSER ERROR" err
    Right (ErrorResponse err) ->
      makeErrorResponse Nothing err "SERVER ERROR"
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
  rawResponse <- liftIO $ Network.sendRequestEither config
                    Network.Request
                      { Network.requestFrom = receiver
                      , Network.requestTo = "SERVER"
                      , Network.requestType = Network.Poll
                      , Network.requestPacket = ""
                      }
  case rawResponse of
    Left err -> do
      setNetworkStatus $ NetworkError err
      return []
    Right response -> do
      processResponse Nothing response
