{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.CPDLC.Message
where

import Web.Hoppie.CPDLC.MessageTypes

import Data.ByteString (ByteString)

data CPDLCMessage =
  CPDLCMessage
    { cpdlcMIN :: Word
    , cpdlcMRN :: Maybe Word
    , cpdlcReplyOpts :: ReplyOpts
    , cpdlcType :: CPDLCType
    }
    deriving (Show, Read, Eq)

type CPDLCType = ByteString
