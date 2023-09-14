{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie
where

import Web.Hoppie.Network

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS

sendTestRequest :: String -> IO ()
sendTestRequest l = do
  let rq = Request
            { requestFrom = "LSGG"
            , requestTo = "SERVER"
            , requestType = Peek
            , requestPacket = ""
            }
  rp <- sendRequest Config { url = defURL, logon = Char8.pack l } rq
  BS.putStr rp
  putStrLn ""
