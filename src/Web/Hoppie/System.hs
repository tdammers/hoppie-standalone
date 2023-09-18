module Web.Hoppie.System
( module Web.Hoppie.System
, module Web.Hoppie.Trans
, TypedMessage (..)
, TypedPayload (..)
, CPDLCMessage (..)
, ReplyOpts (..)
, CPDLCPart (..)
, WithMeta (..)
, Config (..)
)
where

import Web.Hoppie.Trans
import Data.ByteString (ByteString)
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Concurrent.Async (race_)
import Control.Monad.Reader

runSystem :: ByteString
          -> Config
          -> (WithMeta UplinkStatus TypedMessage -> Hoppie ())
          -> (NetworkStatus -> Hoppie ())
          -> (Maybe ByteString -> Hoppie ())
          -> ((TypedMessage -> Hoppie ()) -> Hoppie ())
          -> IO ()
runSystem callsign config onUplink onNetworkStatus onCpdlcLogon runUI = do
  env <- makeHoppieEnv callsign config
  race_
    (runHoppieTWith env (runUI send'))
    (runHoppieTWith env runPolling)
  where
    withStatusCheck action = do
      ns <- asks hoppieNetworkStatus >>= lift . readMVar
      da <- asks hoppieCpdlcDataAuthorities >>= lift . readMVar
      retval <- action
      ns' <- asks hoppieNetworkStatus >>= lift . readMVar
      da' <- asks hoppieCpdlcDataAuthorities >>= lift . readMVar
      when (ns /= ns') (onNetworkStatus ns')
      when (da /= da') (onCpdlcLogon $ currentDataAuthority da')
      return retval

    send' tm = do
      messageIDs <- withStatusCheck $ send tm
      forM_ messageIDs $ \messageID -> do
        msgMay <- getUplink messageID
        forM_ msgMay onUplink

    runPolling = forever $ do
      messageIDs <- withStatusCheck poll
      pollingIntervalVar <- asks hoppieFastPollingCounter

      forM_ messageIDs $ \messageID -> do
        msgMay <- getUplink messageID
        forM_ msgMay onUplink

      fastPolling <- asks (configFastPollingInterval . hoppieNetworkConfig)
      slowPolling <- asks (configPollingInterval . hoppieNetworkConfig)
      liftIO $ do
        counter <- takeMVar pollingIntervalVar
        let interval = if counter > 0 then fastPolling else slowPolling
        putMVar pollingIntervalVar (max 0 $ pred counter)
        threadDelay $ interval * 1000000
