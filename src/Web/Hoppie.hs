{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Web.Hoppie
where

import Web.Hoppie.Trans
import Web.Hoppie.Network
import Web.Hoppie.Response
import Web.Hoppie.CPDLC.Message
import Web.Hoppie.CPDLC.MessageTypes

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.Printf
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import System.IO
import Data.Word

mkConfig :: String -> Config
mkConfig l =
  Config
    { configURL = defURL
    , configLogon = BS8.pack l
    , configFastPollingInterval = 20
    , configPollingInterval = 60
    }

sendTestPeekRequest :: String -> IO ByteString
sendTestPeekRequest l = do
  let rq = Request
            { requestFrom = "EHAM"
            , requestTo = "SERVER"
            , requestType = Peek
            , requestPacket = ""
            }
  rp <- sendRequest (mkConfig l) rq
  BS.putStr rp
  putStrLn ""
  return rp

sendTestInfoRequest :: String -> IO ByteString
sendTestInfoRequest l = do
  let rq = Request
            { requestFrom = "KLM123"
            , requestTo = "SERVER"
            , requestType = Inforeq
            , requestPacket = "VATATIS EDDL"
            }
  sendRequest (mkConfig l) rq

runNetworkTest :: String -> IO ()
runNetworkTest l = do
  let rp = "ok {9716115 EJU66 telex {REQUEST PREDEP CLEARANCE EJU66 A320 TO EHAM AT EGKK STAND 552 ATIS H}} {9716187 DLH4MN telex {REQUEST PREDEP CLEARANCE DLH4MN A20N TO EDDM AT EHAM STAND C10 ATIS B}} {9716198 DLH4MN cpdlc {/data2/1/1831/N/ROGER}} {9716302 KLM1135 telex {REQUEST PREDEP CLEARANCE KLM1135 B78X TO EKCH AT EHAM STAND D3 ATIS D}} {9716315 KLM1135 cpdlc {/data2/1/1832/N/ROGER}} {9716380 EJU87AT telex {REQUEST PREDEP CLEARANCE EJU87AT A319 TO EGPF AT EHAM STAND H1 ATIS E}} {9716392 EJU87AT cpdlc {/data2/2/1833/N/ROGER}} {9716446 EJU57QZ telex {REQUEST PREDEP CLEARANCE EJU57QZ A320 TO EGPH AT EHAM STAND H6 ATIS E}} {9716457 EJU57QZ cpdlc {/data2/1/1834/N/ROGER}} {9717381 EFW6J telex {REQUEST PREDEP CLEARANCE EFW6J A21N TO EGKK AT EHAM STAND D46R ATIS J}} {9717392 KLM telex {REQUEST PREDEP CLEARANCE KLM A20N TO ESSA AT EHAM STAND B15 ATIS J}} {9719620 TRA16X telex {REQUEST PREDEP CLEARANCE TRA16X B738 TO EVRA AT EHAM STAND C09 ATIS C}} "
  -- let rp = "error {Unsupported}"
  -- rp <- sendTestPeekRequest l
  pr <- either error return $ parseResponse rp
  -- forM_ (responseMessages pr) $ \msg -> do
  --   printf "%8s %-8s %-8s %s\n"
  --     (maybe ("n/a" :: String) (printf "%8i") $ messageID msg)
  --     (BS8.unpack $ messageCallsign msg)
  --     (map toLower . show $ messageType msg)
  --     (BS8.unpack $ messagePayload msg)
  case pr of
    ErrorResponse err -> do
      putStrLn "--------- PROTOCOL ERROR ---------"
      BS8.putStrLn err
    Response messages -> do
      forM_ messages $ \msg -> do
        let tm = toTypedUplink msg
        case typedMessagePayload tm of
          TelexPayload body -> do
            putStrLn "--------- TELEX ---------"
            printf "%8s %-8s\n" (maybe "n/a" show $ typedMessageID tm) (BS8.unpack $ typedMessageCallsign tm)
            BS8.putStrLn body
          InfoPayload body -> do
            putStrLn "--------- INFO ---------"
            BS8.putStrLn body
          UnsupportedPayload ty body -> do
            putStrLn "--------- UNSUPPORTED ---------"
            printf "%-8s %s\n" ("Type:" :: String) (show ty)
            BS8.putStrLn body
          ErrorPayload ty body err -> do
            putStrLn "--------- PAYLOAD ERROR ---------"
            putStrLn err
            BS8.putStrLn body
          CPDLCPayload cpdlc -> do
            putStrLn "--------- CPDLC ---------"
            printf "%8s %-8s\n" (maybe "n/a" show $ typedMessageID tm) (BS8.unpack $ typedMessageCallsign tm)
            printf "%-8s %4i\n" ("MIN:" :: String) (cpdlcMIN cpdlc)
            printf "%-8s %4s\n" ("MRN:" :: String) (maybe "-" show $ cpdlcMRN cpdlc)
            printf "%-8s %4s\n" ("RA:" :: String) (BS8.unpack . raToBS $ cpdlcReplyOpts cpdlc)
            printf "%-8s\n" ("PARTS:" :: String)
            forM_ (cpdlcParts cpdlc) $ \part -> do
              putStrLn "  ------------------------"
              printf "  %-8s %-6s\n"
                ("TYPE:" :: String)
                (BS8.unpack $ cpdlcType part)
              printf "  %-8s %s\n"
                ("ARGS:" :: String)
                (intercalate ", " $ map BS8.unpack $ cpdlcArgs part)
              printf "  %-8s %s\n"
                ("TEXT:" :: String)
                (BS8.unpack $ renderMessage allMessageTypes (cpdlcType part) (cpdlcArgs part))
            putStrLn "  ------------------------"
        
  -- runSmokeTests
