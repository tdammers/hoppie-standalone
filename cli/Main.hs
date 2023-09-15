{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Hoppie.System

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = do
  logon <- fromMaybe (error "No logon configured") <$> lookupEnv "HOPPIE_LOGON"
  callsign <- fromMaybe "TEST" <$> lookupEnv "HOPPIE_CALLSIGN"
  url <- fromMaybe defURL <$> lookupEnv "HOPPIE_URL"
  let config = Config (BS8.pack logon) url
  print config
  runSystem (BS8.pack callsign) config handleUplink hoppieMain

handleUplink :: WithMeta UplinkStatus TypedMessage -> Hoppie ()
handleUplink = liftIO . print

hoppieMain :: (TypedMessage -> Hoppie ()) -> Hoppie ()
hoppieMain sendMessage = do
  liftIO $ putStrLn "Hello!"
  forever $ do
    liftIO $ putStr "SERVER> "
    liftIO $ hFlush stdout
    msg <- liftIO $ BS8.pack <$> getLine
    sendMessage (TypedMessage Nothing "SERVER" $ InfoPayload msg)
