{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.Trans
where

import qualified Web.Hoppie.Network as Network
import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.Response

import Control.Monad
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS8
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar

type HoppieT = ReaderT HoppieState

type Hoppie = HoppieT IO

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

data HoppieState =
  HoppieState
    { hoppieNetworkConfig :: !Network.Config
    , hoppieCallsign :: !ByteString
    , hoppieUplinks :: !(MVar (Map Word (WithMeta UplinkStatus TypedMessage)))
    , hoppieDownlinks :: !(MVar (Map Word (WithMeta DownlinkStatus TypedMessage)))
    , hoppieNextUID :: !(MVar Word)
    }

saveUplink :: MonadIO m => WithMeta UplinkStatus TypedMessage -> HoppieT m ()
saveUplink tsm = do
  uplinksVar <- asks hoppieUplinks
  liftIO $ modifyMVar_ uplinksVar $ return . Map.insert (metaUID tsm) tsm

saveDownlink :: MonadIO m => WithMeta DownlinkStatus TypedMessage -> HoppieT m ()
saveDownlink tsm = do
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

makeUID :: MonadIO m => HoppieT m Word
makeUID = do
  uidVar <- asks hoppieNextUID
  liftIO $ modifyMVar uidVar $ \uid -> do
    return (succ uid, uid)

send :: MonadIO m => TypedMessage -> HoppieT m ()
send tm = do
  ts <- liftIO getCurrentTime
  uid <- makeUID
  let rq = WithMeta uid UnsentDownlink ts tm
  config <- asks hoppieNetworkConfig
  sender <- asks hoppieCallsign
  saveDownlink rq
  rawResponse <- liftIO $ Network.sendRequest config (toUntypedRequest sender tm)
  processResponse (Just uid) rawResponse

processResponse :: MonadIO m => Maybe Word -> ByteString -> HoppieT m ()
processResponse uidMay rawResponse = do
  ts <- liftIO getCurrentTime
  case parseResponse rawResponse of
    Left err ->
      error err
    Right (ErrorResponse err) ->
      error (BS8.unpack err)
    Right (Response []) -> do
      return ()
    Right (Response messages) -> do
      forM_ messages $ \msg -> do
        uid <- makeUID
        maybe
          (return ())
          (\parentUID -> setDownlinkStatus parentUID SentDownlink)
          uidMay
        saveUplink (WithMeta uid NewUplink ts $ toTypedMessage msg)

poll :: MonadIO m => HoppieT m ()
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
