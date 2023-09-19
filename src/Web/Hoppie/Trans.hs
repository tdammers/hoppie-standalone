{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Web.Hoppie.CPDLC.MessageTypes (ReplyOpts (..), allMessageTypes)
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.Response
  ( TypedMessage (..)
  , TypedPayload (..)
  , MessageType (..)
  , Response (..)
  , toUntypedRequest
  , toTypedUplink
  , parseResponse
  )

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
    , hoppieCpdlcUplinks :: !(MVar (Map (ByteString, Word) Word))
    , hoppieCpdlcDownlinks :: !(MVar (Map Word Word))
    , hoppieCpdlcUplinksByMRN :: !(MVar (Map (ByteString, Word) Word))
    , hoppieCpdlcDownlinksByMRN :: !(MVar (Map (ByteString, Word) Word))
    , hoppieCpdlcNextMIN :: !(MVar Word)
    , hoppieCpdlcDataAuthorities :: !(MVar DataAuthorities)
    , hoppieNextUID :: !(MVar Word)
    , hoppieNetworkStatus :: !(MVar NetworkStatus)
    , hoppieFastPollingCounter :: !(MVar Int)
    }

data DataAuthorities =
  DataAuthorities
    { currentDataAuthority :: Maybe ByteString
    , nextDataAuthority :: Maybe ByteString
    , logonDataAuthority :: Maybe ByteString
    }
    deriving (Show, Eq)

defDataAuthorities :: DataAuthorities
defDataAuthorities =
  DataAuthorities
    { currentDataAuthority = Nothing
    , nextDataAuthority = Nothing
    , logonDataAuthority = Nothing
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
    <*> newMVar mempty
    <*> newMVar mempty
    <*> newMVar 0
    <*> newMVar defDataAuthorities
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
  | RepliedDownlink
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data UplinkStatus
  = NewUplink
  | OldUplink
  | OpenUplink
  | RepliedUplink
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

isCPDLC :: TypedMessage -> Bool
isCPDLC tm = case typedMessagePayload tm of
  CPDLCPayload {} -> True
  _ -> False

messageIsCPDLC :: HoppieMessage -> Bool
messageIsCPDLC = isCPDLC . messagePayload

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
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.insert (metaUID tsm) tsm
  forM_ (messageCPDLCPayload tsm) $ \cpdlc -> do
    cpdlcUplinksVar <- asks hoppieCpdlcUplinks
    cpdlcDownlinksVar <- asks hoppieCpdlcDownlinks
    let callsign = typedMessageCallsign (payload tsm)
    liftIO $ modifyMVar_ cpdlcUplinksVar $ return . Map.insert (callsign, cpdlcMIN cpdlc) (metaUID tsm)
    forM_ (cpdlcMRN cpdlc) $ \mrn -> do
      cpdlcUplinksByMRNVar <- asks hoppieCpdlcUplinksByMRN
      liftIO $ modifyMVar_ cpdlcUplinksByMRNVar $ return . Map.insert (callsign, mrn) (metaUID tsm)
      parentDownlinkMay <- liftIO $ Map.lookup mrn <$> readMVar cpdlcDownlinksVar
      forM_ parentDownlinkMay $ \parentUID -> do
        setDownlinkStatus parentUID RepliedDownlink
    forM_ (cpdlcParts cpdlc) $ \part -> do
      case cpdlcType part of
        -- LOGON ACCEPTED
        "HPPU-1" -> cpdlcAcceptLogon callsign (cpdlcMIN cpdlc)
        -- HANDOVER
        "HPPU-2" -> cpdlcHandover callsign (cpdlcArgs part) (cpdlcMIN cpdlc)
        -- LOGOFF
        "HPPU-3" -> cpdlcAtcLogoff callsign (cpdlcMIN cpdlc)
        -- Others are just processed normally
        _ -> return ()

cpdlcSendLL :: MonadIO m => ByteString -> Maybe Word -> ByteString -> [ByteString] -> HoppieT m ()
cpdlcSendLL recipient mrnMay tyID args = do
  config <- asks hoppieNetworkConfig
  myCallsign <- asks hoppieCallsign
  downlinkMIN <- makeMIN
  uid <- makeUID
  let tm = TypedMessage (Just uid) recipient . CPDLCPayload $ CPDLCMessage
              { cpdlcMIN = downlinkMIN
              , cpdlcMRN = mrnMay
              , cpdlcReplyOpts = maybe ReplyN CPDLC.msgReplyOpts $ Map.lookup tyID allMessageTypes
              , cpdlcParts =
                  [ CPDLCPart
                      { cpdlcType = tyID
                      , cpdlcArgs = args
                      }
                  ]
              }
  rawResponse <- liftIO $ Network.sendRequestEither config (toUntypedRequest myCallsign tm)
  void $ handleRawResponse (Just uid) rawResponse

cpdlcLogon :: MonadIO m => ByteString -> HoppieT m ()
cpdlcLogon callsign = do
  cpdlcSendLL callsign Nothing "HPPD-1" []
  daVar <- asks hoppieCpdlcDataAuthorities
  liftIO $ modifyMVar_ daVar $ \s -> return s
    { currentDataAuthority = Nothing
    , nextDataAuthority = Just callsign
    , logonDataAuthority = Nothing
    }

cpdlcCancelLogon :: MonadIO m => ByteString -> HoppieT m ()
cpdlcCancelLogon callsign = do
  cpdlcSendLL callsign Nothing "HPPD-2" []
  daVar <- asks hoppieCpdlcDataAuthorities
  liftIO $ modifyMVar_ daVar $ \s -> return s
    { currentDataAuthority = Nothing
    , nextDataAuthority = Nothing
    , logonDataAuthority = Just callsign
    }

cpdlcPilotLogoff :: MonadIO m => ByteString -> HoppieT m ()
cpdlcPilotLogoff callsign = do
  cpdlcSendLL callsign Nothing "HPPD-2" []
  daVar <- asks hoppieCpdlcDataAuthorities
  liftIO $ modifyMVar_ daVar $ \s -> return s
    { currentDataAuthority = Nothing
    , nextDataAuthority = Nothing
    , logonDataAuthority = Just callsign
    }

cpdlcAcceptLogon :: MonadIO m => ByteString -> Word -> HoppieT m ()
cpdlcAcceptLogon callsign uplinkMIN = do
  daVar <- asks hoppieCpdlcDataAuthorities
  da <- liftIO $ takeMVar daVar
  if nextDataAuthority da == Just callsign || logonDataAuthority da == Just callsign then do
    let da' = da
                { currentDataAuthority = Just callsign
                , nextDataAuthority = Nothing
                , logonDataAuthority = Nothing
                }
    liftIO $ putMVar daVar da'
  else do
    liftIO $ putMVar daVar da
    cpdlcSendLL callsign (Just uplinkMIN) "SYSD-3" []

cpdlcAtcLogoff :: MonadIO m => ByteString -> Word -> HoppieT m ()
cpdlcAtcLogoff callsign uplinkMIN = do
  daVar <- asks hoppieCpdlcDataAuthorities
  da <- liftIO $ takeMVar daVar
  if currentDataAuthority da == Just callsign then do
    let da' = da
                { currentDataAuthority = Nothing
                , nextDataAuthority = Nothing
                , logonDataAuthority = Nothing
                }
    liftIO $ putMVar daVar da'
  else do
    liftIO $ putMVar daVar da
    cpdlcSendLL callsign (Just uplinkMIN) "SYSD-3" []

cpdlcHandover :: MonadIO m => ByteString -> [ByteString] -> Word -> HoppieT m ()
cpdlcHandover callsign args uplinkMIN = do
  daVar <- asks hoppieCpdlcDataAuthorities
  da <- liftIO $ takeMVar daVar
  if currentDataAuthority da /= Just callsign then do
    liftIO $ putMVar daVar da
    cpdlcSendLL callsign (Just uplinkMIN) "SYSD-3" []
  else case args of
    [nextDA] -> do
      liftIO $ putMVar daVar da { nextDataAuthority = Just nextDA }
      cpdlcSendLL callsign (Just uplinkMIN) "HPPD-1" []
    _ -> do
      liftIO $ putMVar daVar da
      cpdlcSendLL callsign (Just uplinkMIN) "SYSD-1" ["MISSING ARGUMENT"]

saveDownlink :: MonadIO m => WithMeta DownlinkStatus TypedMessage -> HoppieT m ()
saveDownlink tsm = do
  downlinksVar <- asks hoppieDownlinks
  liftIO $ modifyMVar_ downlinksVar $ return . Map.insert (metaUID tsm) tsm
  forM_ (messageCPDLCPayload tsm) $ \cpdlc -> do
    cpdlcUplinksVar <- asks hoppieCpdlcUplinks
    cpdlcDownlinksVar <- asks hoppieCpdlcDownlinks
    liftIO $ modifyMVar_ cpdlcDownlinksVar $ return . Map.insert (cpdlcMIN cpdlc) (metaUID tsm)
    forM_ (cpdlcMRN cpdlc) $ \mrn -> do
      let callsign = typedMessageCallsign (payload tsm)
      cpdlcDownlinksByMRNVar <- asks hoppieCpdlcDownlinksByMRN
      liftIO $ modifyMVar_ cpdlcDownlinksByMRNVar $ return . Map.insert (callsign, mrn) (metaUID tsm)
      parentUplinkMay <- liftIO $ Map.lookup (callsign, mrn) <$> readMVar cpdlcUplinksVar
      forM_ parentUplinkMay $ \parentUID -> do
        setUplinkStatus parentUID RepliedUplink

messageCPDLCPayload :: WithMeta a TypedMessage -> Maybe CPDLCMessage
messageCPDLCPayload tsm = case typedMessagePayload (payload tsm) of
  CPDLCPayload cpdlc -> Just cpdlc
  _ -> Nothing

getCpdlcParentUplink :: MonadIO m
                     => WithMeta DownlinkStatus TypedMessage
                     -> HoppieT m (Maybe (WithMeta UplinkStatus TypedMessage))
getCpdlcParentUplink tsm = do
  cpdlcUplinks <- asks hoppieCpdlcUplinks >>= liftIO . readMVar
  uplinks <- asks hoppieUplinks >>= liftIO . readMVar
  let callsign = typedMessageCallsign (payload tsm)
  return $ do
    cpdlc <- messageCPDLCPayload tsm
    mrn <- cpdlcMRN cpdlc
    uid <- Map.lookup (callsign, mrn) cpdlcUplinks
    Map.lookup uid uplinks

getCpdlcParentDownlink :: MonadIO m
                     => WithMeta UplinkStatus TypedMessage
                     -> HoppieT m (Maybe (WithMeta DownlinkStatus TypedMessage))
getCpdlcParentDownlink tsm = do
  cpdlcDownlinks <- asks hoppieCpdlcDownlinks >>= liftIO . readMVar
  downlinks <- asks hoppieDownlinks >>= liftIO . readMVar
  return $ do
    cpdlc <- messageCPDLCPayload tsm
    mrn <- cpdlcMRN cpdlc
    uid <- Map.lookup mrn cpdlcDownlinks
    Map.lookup uid downlinks

getCpdlcChildUplink :: MonadIO m
                     => WithMeta DownlinkStatus TypedMessage
                     -> HoppieT m (Maybe (WithMeta UplinkStatus TypedMessage))
getCpdlcChildUplink tsm = do
  cpdlcUplinks <- asks hoppieCpdlcUplinksByMRN >>= liftIO . readMVar
  uplinks <- asks hoppieUplinks >>= liftIO . readMVar
  let callsign = typedMessageCallsign (payload tsm)
  return $ do
    cpdlc <- messageCPDLCPayload tsm
    uid <- Map.lookup (callsign, cpdlcMIN cpdlc) cpdlcUplinks
    Map.lookup uid uplinks

getCpdlcChildDownlink :: MonadIO m
                     => WithMeta UplinkStatus TypedMessage
                     -> HoppieT m (Maybe (WithMeta DownlinkStatus TypedMessage))
getCpdlcChildDownlink tsm = do
  cpdlcDownlinks <- asks hoppieCpdlcDownlinksByMRN >>= liftIO . readMVar
  downlinks <- asks hoppieDownlinks >>= liftIO . readMVar
  let callsign = typedMessageCallsign (payload tsm)
  return $ do
    cpdlc <- messageCPDLCPayload tsm
    uid <- Map.lookup (callsign, cpdlcMIN cpdlc) cpdlcDownlinks
    Map.lookup uid downlinks


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

getCPDLCUplink :: MonadIO m => ByteString -> Word -> HoppieT m (Maybe (WithMeta UplinkStatus TypedMessage))
getCPDLCUplink callsign cMIN = do
  cpdlcUplinkVar <- asks hoppieCpdlcUplinks
  uidMay <- Map.lookup (callsign, cMIN) <$> liftIO (readMVar cpdlcUplinkVar)
  case uidMay of
    Nothing -> return Nothing
    Just uid -> getUplink uid

getCPDLCDownlink :: MonadIO m => Word -> HoppieT m (Maybe (WithMeta DownlinkStatus TypedMessage))
getCPDLCDownlink cMIN = do
  cpdlcDownlinkVar <- asks hoppieCpdlcDownlinks
  uidMay <- Map.lookup cMIN <$> liftIO (readMVar cpdlcDownlinkVar)
  case uidMay of
    Nothing -> return Nothing
    Just uid -> getDownlink uid

makeUID :: MonadIO m => HoppieT m Word
makeUID = do
  uidVar <- asks hoppieNextUID
  liftIO $ modifyMVar uidVar $ \uid -> do
    return (succ uid, uid)

makeMIN :: MonadIO m => HoppieT m Word
makeMIN = do
  minVar <- asks hoppieCpdlcNextMIN
  liftIO $ modifyMVar minVar $ \minVal -> do
    return (succ minVal, minVal)

send :: MonadIO m => TypedMessage -> HoppieT m [Word]
send tm = do
  ts <- liftIO getCurrentTime
  uid <- makeUID
  let rq = WithMeta uid UnsentDownlink ts tm
  config <- asks hoppieNetworkConfig
  sender <- asks hoppieCallsign
  saveDownlink rq
  rawResponse <- liftIO $ Network.sendRequestEither config (toUntypedRequest sender tm)
  handleRawResponse (Just uid) rawResponse

handleRawResponse :: MonadIO m => Maybe Word -> Either String ByteString -> HoppieT m [Word]
handleRawResponse uidMay = \case
  Left err -> do
    forM_ uidMay $ \uid -> setDownlinkStatus uid ErrorDownlink
    setNetworkStatus $ NetworkError err
    return []
  Right response -> do
    forM_ uidMay $ \uid -> setDownlinkStatus uid SentDownlink
    setNetworkStatus NetworkOK
    processResponse uidMay response

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
        saveUplink (WithMeta uid NewUplink ts $ toTypedUplink msg)
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
      setNetworkStatus NetworkOK
      processResponse Nothing response
