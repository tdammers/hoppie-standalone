{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Hoppie.System
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.Input
import Web.Hoppie.MCDU

import Control.Concurrent.STM
import Control.Concurrent.Async (race_)
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import Control.Monad.IO.Class
import Data.Word
import Text.Printf

data MCDUEvent
  = InputCommandEvent InputCommand
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  deriving (Show)

main :: IO ()
main = do
  inputChan <- newTChanIO
  eventChan <- newTChanIO
  logon <- fromMaybe (error "No logon configured") <$> lookupEnv "HOPPIE_LOGON"
  callsign <- fromMaybe "TEST" <$> lookupEnv "HOPPIE_CALLSIGN"
  url <- fromMaybe defURL <$> lookupEnv "HOPPIE_URL"
  let config = Config (BS8.pack logon) url
  runInput inputChan
    `race_`
    runInputPusher inputChan eventChan
    `race_`
    runSystem (BS8.pack callsign) config handleUplink (hoppieMain eventChan)

runInputPusher :: TChan Word8 -> TChan MCDUEvent -> IO ()
runInputPusher inputChan eventChan = forever $ do
  readCommand inputChan >>= atomically . writeTChan eventChan . InputCommandEvent

handleUplink :: WithMeta UplinkStatus TypedMessage -> Hoppie ()
handleUplink = liftIO . print

hoppieMain :: TChan MCDUEvent -> (TypedMessage -> Hoppie ()) -> Hoppie ()
hoppieMain _eventChan _sendMessage = do
  let paint = do
        mcduPrintC (screenW `div` 2) 0 green "MENU"
        mcduPrintLskL 1 "DLK"
        mcduPrintLskR 1 "ATC"
    
  let screenBuf = runMCDUDraw paint emptyMCDUScreenBuffer
  liftIO $ drawMCDU screenBuf
  -- forever $ do
  --   liftIO $ putStr "SERVER> "
  --   liftIO $ hFlush stdout
  --   msg <- liftIO $ BS8.pack <$> getLine
  --   sendMessage (TypedMessage Nothing "SERVER" $ InfoPayload msg)
