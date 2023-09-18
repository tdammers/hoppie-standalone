{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Hoppie.System
import Web.Hoppie.Telex
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.MCDU
import Web.Hoppie.TUI.StringUtil

import Control.Concurrent.STM
import Control.Concurrent.Async (race_)
import Control.Monad
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import Control.Monad.IO.Class
import Data.Word

main :: IO ()
main = do
  inputChan <- newTChanIO
  eventChan <- newTChanIO
  logon <- fromMaybe (error "No logon configured") <$> lookupEnv "HOPPIE_LOGON"
  callsign <- fromMaybe "TEST" <$> lookupEnv "HOPPIE_CALLSIGN"
  url <- fromMaybe defURL <$> lookupEnv "HOPPIE_URL"
  pollingInterval <- maybe 60 read <$> lookupEnv "HOPPIE_POLLING_INTERVAL"
  fastPollingInterval <- maybe 20 read <$> lookupEnv "HOPPIE_FAST_POLLING_INTERVAL"
  let config = Config (BS8.pack logon) url pollingInterval fastPollingInterval
  runInput inputChan
    `race_`
    runInputPusher inputChan eventChan
    `race_`
    runSystem
      (BS8.pack callsign)
      config
      (handleUplink eventChan)
      (handleNetworkStatus eventChan)
      (handleCurrentDataAuthority eventChan)
      (hoppieMain eventChan)

runInputPusher :: TChan Word8 -> TChan MCDUEvent -> IO ()
runInputPusher inputChan eventChan = forever $ do
  readCommand inputChan >>= atomically . writeTChan eventChan . InputCommandEvent

handleUplink :: TChan MCDUEvent -> WithMeta UplinkStatus TypedMessage -> Hoppie ()
handleUplink eventChan = do
  liftIO . atomically . writeTChan eventChan . UplinkEvent

handleNetworkStatus :: TChan MCDUEvent -> NetworkStatus -> Hoppie ()
handleNetworkStatus eventChan = do
  liftIO . atomically . writeTChan eventChan . NetworkStatusEvent

handleCurrentDataAuthority :: TChan MCDUEvent -> Maybe ByteString -> Hoppie ()
handleCurrentDataAuthority eventChan = do
  liftIO . atomically . writeTChan eventChan . CurrentDataAuthorityEvent

hoppieMain :: TChan MCDUEvent -> (TypedMessage -> Hoppie ()) -> Hoppie ()
hoppieMain eventChan sendMessage = do
  runMCDU sendMessage $ do
    loadView mainMenuView
    flushAll
    forever $ do
      ev <- liftIO . atomically $ readTChan eventChan
      handleMCDUEvent mainMenuView dlkMenuView atcMenuView ev

runColoredBSTests :: IO ()
runColoredBSTests = do
  let testStr = ColoredBS
        [ ColoredBSFragment 1 "HELLO WORLD "
        , ColoredBSFragment 2 "\nHOW ARE"
        , ColoredBSFragment 3 " YOU"
        , ColoredBSFragment 4 " THIS IS A FAIRLY LONG STRING WITH "
        , ColoredBSFragment 5 "PLENTY OF WORDS "
        ]
  print (coloredToBS testStr)
  print (coloredToBS $ coloredTake 10 testStr)
  print (coloredToBS $ coloredDrop 10 testStr)
  print (coloredFindIndex isSpace8 testStr)
  print (map coloredToBS $ coloredWordSplit testStr)
  print (map coloredToBS $ coloredLineSplit testStr)
  print (map coloredToBS $ lineWrap 20 testStr)
  print (lineWrap 20 $ coloredToBS testStr)
