module Web.Hoppie.System where

import Web.Hoppie.Trans
import Web.Hoppie.Network (Config)

import Data.ByteString (ByteString)
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Concurrent.Async (race_)

runSystem :: ByteString
          -> Config
          -> (WithMeta UplinkStatus TypedMessage -> Hoppie ())
          -> Hoppie ()
          -> IO ()
runSystem callsign config onUplink runUI = do
  env <- makeHoppieEnv callsign config
  race_
    (runHoppieTWith env runUI)
    (runHoppieTWith env runPolling)
  where
    runPolling = forever $ do
      messageIDs <- poll
      forM_ messageIDs $ \messageID -> do
        msgMay <- getUplink messageID
        forM_ msgMay onUplink
      liftIO $ threadDelay $ 60 * 1000000
