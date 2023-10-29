{-# LANGUAGE FlexibleContexts #-}

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
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

runSystem :: ByteString
          -> Config
          -> HoppieHooks IO
          -> ((TypedMessage -> Hoppie ()) -> Hoppie ())
          -> IO ()
runSystem callsign config hooks runUI = do
  env <- makeHoppieEnv hooks callsign config
  runHoppieTWith env $ setAtisSource (configAtisSource config)
  sendQueue <- newTChanIO
  timer <- newMVar 0

  let send' tm = do
        pollingIntervalVar <- asks hoppieFastPollingCounter
        -- increase polling frequency for a while after sending a message
        liftIO $ do
          modifyMVar_ pollingIntervalVar $ const . return $ 10
          atomically $ writeTChan sendQueue tm

      withStatusCheck action = do
        ns <- asks hoppieNetworkStatus >>= lift . readMVar
        da <- asks hoppieCpdlcDataAuthorities >>= lift . readMVar
        retval <- action
        ns' <- asks hoppieNetworkStatus >>= lift . readMVar
        da' <- asks hoppieCpdlcDataAuthorities >>= lift . readMVar
        when (ns /= ns') (onNetworkStatus hooks ns')
        when (da /= da') (onCpdlcLogon hooks $ currentDataAuthority da')
        return retval

      runPolling = forever $ do
        pollingIntervalVar <- asks hoppieFastPollingCounter
        fastPolling <- asks (configFastPollingInterval . hoppieNetworkConfig)
        slowPolling <- asks (configPollingInterval . hoppieNetworkConfig)

        t <- liftIO $ takeMVar timer
        queueEmpty <- liftIO . atomically $ isEmptyTChan sendQueue
        t' <- if queueEmpty then do
          counter <- liftIO $ takeMVar pollingIntervalVar
          let interval = if counter > 0 then fastPolling else slowPolling
          if t >= interval then do
            -- liftIO $ putStrLn $ "POLL" ++ show counter
            void $ withStatusCheck poll
            liftIO $ putMVar pollingIntervalVar (max 0 $ counter - 1)
            return $ t - interval
          else do
            -- liftIO $ putStrLn $ "TICK" ++ show (interval - t)
            liftIO $ putMVar pollingIntervalVar counter
            return t
        else do
          if t >= 15 then do
            -- liftIO $ putStrLn "SEND"
            tm <- liftIO $ atomically (readTChan sendQueue)
            void $ withStatusCheck $ send tm
            return $ t - 15
          else do
            -- liftIO $ putStrLn "TICK"
            return t
        liftIO $ putMVar timer (t' + 1)
        liftIO $ threadDelay 1000000
  race_
    (runHoppieTWith env (runUI send'))
    (runHoppieTWith env runPolling)
