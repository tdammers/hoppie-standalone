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
          -> ((TypedMessage -> Hoppie ()) -> Hoppie ())
          -> IO ()
runSystem callsign config onUplink onNetworkStatus runUI = do
  env <- makeHoppieEnv callsign config
  race_
    (runHoppieTWith env (runUI send'))
    (runHoppieTWith env runPolling)
  where
    withStatusCheck action = do
      ns <- asks hoppieNetworkStatus >>= lift . readMVar
      retval <- action
      ns' <- asks hoppieNetworkStatus >>= lift . readMVar
      when (ns /= ns') (onNetworkStatus ns')
      return retval

    send' tm = do
      messageIDs <- withStatusCheck $ send tm
      forM_ messageIDs $ \messageID -> do
        msgMay <- getUplink messageID
        forM_ msgMay onUplink

    runPolling = forever $ do
      messageIDs <- withStatusCheck poll
      forM_ messageIDs $ \messageID -> do
        msgMay <- getUplink messageID
        forM_ msgMay onUplink
      liftIO $ threadDelay $ 60 * 1000000
