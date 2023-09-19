{-# LANGUAGE NamedFieldPuns #-}

module Web.Hoppie.System
( module Web.Hoppie.System
, module Web.Hoppie.Trans
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
          -> HoppieHooks IO
          -> ((TypedMessage -> Hoppie ()) -> Hoppie ())
          -> IO ()
runSystem callsign config hooks runUI = do
  env <- makeHoppieEnv hooks callsign config
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
      when (ns /= ns') (onNetworkStatus hooks $ ns')
      when (da /= da') (onCpdlcLogon hooks $ currentDataAuthority da')
      return retval

    send' tm = do
      pollingIntervalVar <- asks hoppieFastPollingCounter
      -- increase polling frequency for a while after sending a message
      liftIO $ modifyMVar_ pollingIntervalVar $ const . return $ 10
      void $ withStatusCheck $ send tm

    runPolling = forever $ do
      void $ withStatusCheck poll
      pollingIntervalVar <- asks hoppieFastPollingCounter

      fastPolling <- asks (configFastPollingInterval . hoppieNetworkConfig)
      slowPolling <- asks (configPollingInterval . hoppieNetworkConfig)
      liftIO $ do
        counter <- takeMVar pollingIntervalVar
        let interval = if counter > 0 then fastPolling else slowPolling
        putMVar pollingIntervalVar (max 0 $ pred counter)
        threadDelay $ interval * 1000000
