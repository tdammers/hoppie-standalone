{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.Trans
( module Web.Hoppie.Trans
, TypedMessage (..)
, TypedPayload (..)
, typedPayloadTypeBS
, CPDLCMessage (..)
, ReplyOpts (..)
, CPDLCPart (..)
, WithMeta (..)
, Network.defURL
, Network.Config (..)
, Network.AtisSource (..)
)
where

import Web.Hoppie.CPDLC.Message (CPDLCMessage (..), CPDLCPart (..))
import Web.Hoppie.CPDLC.MessageTypes (ReplyOpts (..), allMessageTypes)
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import qualified Web.Hoppie.Network as Network
import Web.Hoppie.Response
  ( TypedMessage (..)
  , TypedPayload (..)
  , typedPayloadTypeBS
  , MessageType (..)
  , Response (..)
  , toUntypedRequest
  , toTypedUplink
  , parseResponse
  )
import Web.Hoppie.StringUtil
import qualified Web.Vatsim as Vatsim

import Control.Applicative
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), (.:?))
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)

type HoppieT m = ReaderT (HoppieEnv m) m

type Hoppie = HoppieT IO

data HoppieEnv m =
  HoppieEnv
    { hoppieNetworkConfig :: !Network.Config
    , hoppieHooks :: !(HoppieHooks m)
    , hoppieCallsign :: !(MVar ByteString)
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
    , hoppieAtisSrc :: !(MVar Network.AtisSource)
    , hoppieVatsimThread :: !(MVar (Async ()))
    , hoppieVatsimFeedVar :: !(MVar Vatsim.Datafeed)
    }

data HoppieHooks m =
  HoppieHooks
    { onUplink :: WithMeta UplinkStatus TypedMessage -> HoppieT m ()
    , onDownlink :: WithMeta DownlinkStatus TypedMessage -> HoppieT m ()
    , onNetworkStatus :: NetworkStatus -> HoppieT m ()
    , onCpdlcLogon :: Maybe ByteString -> HoppieT m ()
    , onDataUpdated :: HoppieT m ()
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

instance ToJSON DataAuthorities where
  toJSON da = JSON.object $
    [ "current" .= decodeUtf8 x | x <- maybeToList (currentDataAuthority da) ] ++
    [ "next" .= decodeUtf8 x | x <- maybeToList (nextDataAuthority da) ] ++
    [ "logon" .= decodeUtf8 x | x <- maybeToList (logonDataAuthority da) ]

instance FromJSON DataAuthorities where
  parseJSON = JSON.withObject "DataAuthorities" $ \obj ->
    DataAuthorities
      <$> (fmap encodeUtf8 <$> obj .:? "current")
      <*> (fmap encodeUtf8 <$> obj .:? "next")
      <*> (fmap encodeUtf8 <$> obj .:? "logon")

data NetworkStatus
  = NetworkOK
  | NetworkError String
  deriving (Show, Eq)

makeHoppieEnv :: MonadIO m => HoppieHooks m -> ByteString -> Network.Config -> m (HoppieEnv m)
makeHoppieEnv hooks callsign config =
  liftIO $ HoppieEnv
    config
    hooks
    <$> newMVar callsign
    <*> newMVar mempty
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
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

runHoppieTWith :: HoppieEnv m -> HoppieT m a -> m a
runHoppieTWith = flip runReaderT

setAtisSource :: MonadIO m => Network.AtisSource -> HoppieT m ()
setAtisSource src = do
  srcVar <- asks hoppieAtisSrc
  currentSrc <- liftIO $ tryTakeMVar srcVar
  case currentSrc of
    Just Network.AtisSourceVatsimDatafeed -> do
      stopVatsimFetcher
    _ ->
      return ()
  case src of
    Network.AtisSourceVatsimDatafeed -> do
      startVatsimFetcher
    _ ->
      return ()
  liftIO $ putMVar srcVar src

getAtisSource :: MonadIO m => HoppieT m (Maybe Network.AtisSource)
getAtisSource = do
  srcVar <- asks hoppieAtisSrc
  liftIO $ tryReadMVar srcVar

startVatsimFetcher :: MonadIO m => HoppieT m ()
startVatsimFetcher = do
  threadVar <- asks hoppieVatsimThread
  feedVar <- asks hoppieVatsimFeedVar
  liftIO $ do
    threadMay <- tryTakeMVar threadVar
    forM_ threadMay Async.cancel
    thread <- Vatsim.runDatafeedFetcherWith feedVar
    putMVar threadVar thread

stopVatsimFetcher :: MonadIO m => HoppieT m ()
stopVatsimFetcher = do
  threadVar <- asks hoppieVatsimThread
  liftIO $ do
    threadMay <- tryTakeMVar threadVar
    forM_ threadMay Async.cancel

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
    deriving (Show, Eq)

data HoppieMessage
  = UplinkMessage (WithMeta UplinkStatus TypedMessage)
  | DownlinkMessage (WithMeta DownlinkStatus TypedMessage)


data PersistentHoppieData =
  PersistentHoppieData
    { phdCallsign :: Text
    , phdUplinks :: [WithMeta UplinkStatus TypedMessage]
    , phdDownlinks :: [WithMeta DownlinkStatus TypedMessage]
    , phdNextMIN :: Word
    , phdNextUID :: Word
    , phdCpdlcDataAuthorities :: DataAuthorities
    }
    deriving (Show, Eq)

$(deriveJSON JSON.defaultOptions { JSON.constructorTagModifier = Text.unpack . Text.dropEnd 6 . Text.pack } ''UplinkStatus)
$(deriveJSON JSON.defaultOptions { JSON.constructorTagModifier = Text.unpack . Text.dropEnd 8 . Text.pack } ''DownlinkStatus)
$(deriveJSON JSON.defaultOptions ''WithMeta)
$(deriveJSON JSON.defaultOptions { JSON.fieldLabelModifier = lcfirst . drop 3 } ''PersistentHoppieData)

getPersistentData :: MonadIO m => HoppieT m PersistentHoppieData
getPersistentData =
  PersistentHoppieData
    <$> (decodeUtf8 <$> getCallsign)
    <*> (Map.elems <$> (asks hoppieUplinks >>= liftIO . readMVar))
    <*> (Map.elems <$> (asks hoppieDownlinks >>= liftIO . readMVar))
    <*> (asks hoppieCpdlcNextMIN >>= liftIO . readMVar)
    <*> (asks hoppieNextUID >>= liftIO . readMVar)
    <*> (asks hoppieCpdlcDataAuthorities >>= liftIO . readMVar)

restorePersistentData :: MonadIO m => PersistentHoppieData -> HoppieT m ()
restorePersistentData phd = do
  asks hoppieCallsign >>= \var ->
    liftIO $ modifyMVar_ var (const . return . encodeUtf8 $ phdCallsign phd)
  forM_ (phdUplinks phd) saveUplink
  forM_ (phdDownlinks phd) saveDownlink
  asks hoppieCpdlcNextMIN >>= \var ->
    liftIO $ modifyMVar_ var (const . return $ phdNextMIN phd)
  asks hoppieNextUID >>= \var ->
    liftIO $ modifyMVar_ var (const . return $ phdNextUID phd)
  asks hoppieCpdlcDataAuthorities >>= \var ->
    liftIO $ modifyMVar_ var (const . return $ phdCpdlcDataAuthorities phd)

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

getUplinkMessages :: MonadIO m => HoppieT m [HoppieMessage]
getUplinkMessages = do
  uplinks <- fmap (map UplinkMessage . Map.elems) $
              asks hoppieUplinks >>= liftIO . readMVar
  return $ sortOn messageUID uplinks

getDownlinkMessages :: MonadIO m => HoppieT m [HoppieMessage]
getDownlinkMessages = do
  downlinks <- fmap (map DownlinkMessage . Map.elems) $
              asks hoppieDownlinks >>= liftIO . readMVar
  return $ sortOn messageUID downlinks

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

clearUplinksLog :: MonadIO m => HoppieT m ()
clearUplinksLog = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.filter (isCPDLC . payload)

clearDownlinksLog :: MonadIO m => HoppieT m ()
clearDownlinksLog = do
  downlinksVar <- asks hoppieDownlinks
  liftIO $ modifyMVar_ downlinksVar $ return . Map.filter (isCPDLC . payload)

clearCpdlcUplinksLog :: MonadIO m => HoppieT m ()
clearCpdlcUplinksLog = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.filter (not . isCPDLC . payload)

clearCpdlcDownlinksLog :: MonadIO m => HoppieT m ()
clearCpdlcDownlinksLog = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.filter (not . isCPDLC . payload)

getCallsign :: MonadIO m => HoppieT m ByteString
getCallsign = asks hoppieCallsign >>= liftIO . readMVar

setCallsign :: MonadIO m => ByteString -> HoppieT m ()
setCallsign callsign = do
  asks hoppieCallsign >>= liftIO . flip modifyMVar_ (const . return $ callsign)
  join $ asks (onDataUpdated . hoppieHooks)

cpdlcSendLL :: MonadIO m => ByteString -> Maybe Word -> ByteString -> [ByteString] -> HoppieT m ()
cpdlcSendLL recipient mrnMay tyID args = do
  config <- asks hoppieNetworkConfig
  myCallsign <- getCallsign
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
  sender <- getCallsign
  saveDownlink rq
  sentHook <- asks (onDownlink . hoppieHooks)
  sentHook rq
  atisSrc <- asks hoppieAtisSrc >>= liftIO . readMVar
  rawResponse <- case (atisSrc, typedMessagePayload tm) of
    (Network.AtisSourceVatsimDatafeed, InfoPayload infoStr)
      | "VATATIS " `Text.isPrefixOf` decodeUtf8 infoStr
      -> do
        let station = Text.replace "-" "_" . mconcat . take 1 . drop 1 . Text.words . decodeUtf8 $ infoStr
        feedVar <- asks hoppieVatsimFeedVar
        atisMay <- liftIO $ Vatsim.getCurrentAtis feedVar station
        liftIO $ print atisMay
        case atisMay of
          Nothing -> return $ Left "VATSIM DATA NOT AVAIL"
          Just [] -> return . Right $ "ok {VATSIM info {THIS ATIS IS NOT AVAILABLE}}"
          Just atises -> return . Right $
            "ok" <>
              mconcat [ " {VATSIM info {" <> encodeUtf8 atis <> "}}"
                      | atis <- atises
                      ]


    _ -> do
      liftIO $ Network.sendRequestEither config (toUntypedRequest sender tm)
  liftIO $ print rawResponse
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

{-# ANN processResponse ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

processResponse :: MonadIO m => Maybe Word -> ByteString -> HoppieT m [Word]
processResponse uidMay rawResponse = do
  ts <- liftIO getCurrentTime
  uids <- case parseResponse rawResponse of
    Left err ->
      makeErrorResponse Nothing "PARSER ERROR" err
    Right (ErrorResponse err) ->
      makeErrorResponse Nothing err "SERVER ERROR"
    Right (Response []) -> do
      return []
    Right (Response messages) -> do
      forM messages $ \msg -> do
        uid <- makeUID
        forM_ uidMay $ \parentUID -> 
          setDownlinkStatus parentUID SentDownlink
        let uplink = WithMeta uid NewUplink ts $ toTypedUplink msg
        saveUplink uplink
        recvdHook <- asks (onUplink . hoppieHooks)
        recvdHook uplink
        return uid
  join $ asks (onDataUpdated . hoppieHooks)
  return uids

poll :: MonadIO m => HoppieT m [Word]
poll = do
  config <- asks hoppieNetworkConfig
  receiver <- getCallsign
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
