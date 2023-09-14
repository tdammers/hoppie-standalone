{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Web.Hoppie.CPDLC.MessageTypes
where

import Web.Hoppie.Telex
import Data.Trie (Trie (..))
import qualified Data.Trie as Trie

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Void
import Data.String
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P (decimal)
import Data.List
 
data ArgTy
  = ArgFlAlt
  | ArgSpeed
  | ArgNavpos
  | ArgRoute
  | ArgXpdr
  | ArgCallsign
  | ArgFreq
  | ArgTime
  | ArgDirection 
  | ArgDegrees
  | ArgAtisCode
  | ArgDeviationType
  | ArgEndurance
  | ArgLegtype
  | ArgText
  | ArgInteger
  | ArgReason
  | ArgClearanceType
  | ArgPosrep
  | ArgDistance
  | ArgProcedure
  | ArgSpeedType
  | ArgFacility
  | ArgAltimeter
  | ArgDataAuthority
  | ArgVspeed
  | ArgMinutes
  | ArgLatency
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

argFlAlt :: ArgSpec
argFlAlt = ArgSpec ArgFlAlt False

argSpeed :: ArgSpec
argSpeed = ArgSpec ArgSpeed False

argNavpos :: ArgSpec
argNavpos = ArgSpec ArgNavpos False

argRoute :: ArgSpec
argRoute = ArgSpec ArgRoute False

argXpdr :: ArgSpec
argXpdr = ArgSpec ArgXpdr False

argCallsign :: ArgSpec
argCallsign = ArgSpec ArgCallsign False

argFreq :: ArgSpec
argFreq = ArgSpec ArgFreq False

argTime :: ArgSpec
argTime = ArgSpec ArgTime False

argDirection  :: ArgSpec
argDirection  = ArgSpec ArgDirection  False

argDegrees :: ArgSpec
argDegrees = ArgSpec ArgDegrees False

argAtisCode :: ArgSpec
argAtisCode = ArgSpec ArgAtisCode False

argDeviationType :: ArgSpec
argDeviationType = ArgSpec ArgDeviationType False

argEndurance :: ArgSpec
argEndurance = ArgSpec ArgEndurance False

argLegtype :: ArgSpec
argLegtype = ArgSpec ArgLegtype False

argText :: ArgSpec
argText = ArgSpec ArgText False

argInteger :: ArgSpec
argInteger = ArgSpec ArgInteger False

argReason :: ArgSpec
argReason = ArgSpec ArgReason False

argClearanceType :: ArgSpec
argClearanceType = ArgSpec ArgClearanceType False

argPosrep :: ArgSpec
argPosrep = ArgSpec ArgPosrep False

argDistance :: ArgSpec
argDistance = ArgSpec ArgDistance False

argProcedure :: ArgSpec
argProcedure = ArgSpec ArgProcedure False

argSpeedType :: ArgSpec
argSpeedType = ArgSpec ArgSpeedType False

argFacility :: ArgSpec
argFacility = ArgSpec ArgFacility False

argAltimeter :: ArgSpec
argAltimeter = ArgSpec ArgAltimeter False

argDataAuthority :: ArgSpec
argDataAuthority = ArgSpec ArgDataAuthority False

argVspeed :: ArgSpec
argVspeed = ArgSpec ArgVspeed False

argMinutes :: ArgSpec
argMinutes = ArgSpec ArgMinutes False

argLatency :: ArgSpec
argLatency = ArgSpec ArgLatency False

optArg :: ArgSpec -> ArgSpec
optArg a = a { argOptional = True }

data ArgSpec =
  ArgSpec
    { argTy :: ArgTy
    , argOptional :: Bool
    }
    deriving (Show, Read, Ord, Eq)

data ReplyOpts
  = ReplyY
  | ReplyN
  | ReplyAN
  | ReplyWU
  | ReplyR
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype MessagePattern a =
  MessagePattern
    { messagePatternItems :: [MessagePatternItem a]
    }
  deriving newtype (Show, Read, Eq, Semigroup, Monoid)
  deriving (Functor)

type ArgRef = Word

data MessagePatternItem a
  = MessageLiteral ByteString
  | MessageArgRef a
  deriving (Show, Read, Eq, Ord, Functor)

messagePatternP :: P.Parsec Void ByteString (MessagePattern ArgRef)
messagePatternP = MessagePattern <$> P.many messagePatternItemP

messagePatternItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
messagePatternItemP =
  (argRefItemP <|> litItemP) <* P.space

argRefItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
argRefItemP = do
  void $ P.char (ord8 '$')
  MessageArgRef <$> P.decimal

litItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
litItemP =
  MessageLiteral <$>
    P.takeWhile1P
      (Just "characters")
      (\c -> c /= ord8 '$' && not (isSpace8 c))

instance IsString (MessagePattern ArgRef) where
  fromString = either (error . P.errorBundlePretty) id . P.runParser messagePatternP "input" . BS8.pack

type MessageTypeID = ByteString

data MessageType =
  MessageType
    { msgPattern :: MessagePattern ArgRef
    , msgArgs :: [ArgSpec]
    , msgReplyOpts :: ReplyOpts
    , msgReplies :: [(MessageTypeID, [Maybe ArgRef])]
    }
    deriving (Show, Read)

defMessageType :: MessageType
defMessageType =
  MessageType
    { msgPattern = MessagePattern []
    , msgArgs = []
    , msgReplyOpts = ReplyR
    , msgReplies = []
    }

resolveMessagePatternArgs :: [ArgSpec] -> MessagePattern ArgRef -> MessagePattern ArgSpec
resolveMessagePatternArgs specs = MessagePattern . map (resolveMessagePatternItemArgs specs) . messagePatternItems

resolveMessagePatternItemArgs :: [ArgSpec] -> MessagePatternItem ArgRef -> MessagePatternItem ArgSpec
resolveMessagePatternItemArgs _ (MessageLiteral str) = MessageLiteral str
resolveMessagePatternItemArgs args (MessageArgRef i) = MessageArgRef $ args !! (fromIntegral i - 1)

makeMessageTrie :: Map MessageTypeID MessageType -> Trie (MessagePatternItem ArgSpec) MessageTypeID
makeMessageTrie m =
  foldl'
    (\t (msgID, msgTy) -> Trie.insert (messagePatternItems . resolveMessagePatternArgs (msgArgs msgTy) $ msgPattern msgTy) msgID t)
    mempty (Map.toList m)

pseudoMessageTrie :: Trie (MessagePatternItem ArgSpec) MessageTypeID
pseudoMessageTrie = makeMessageTrie pseudoMessages

uplinkMessageTrie :: Trie (MessagePatternItem ArgSpec) MessageTypeID
uplinkMessageTrie = makeMessageTrie uplinkMessages

pseudoMessages :: Map MessageTypeID MessageType
pseudoMessages = Map.fromList
  [ ( "CONX-1", defMessageType { msgPattern = "LOGON TO $1", msgArgs = [ argFacility ] } )
  , ( "CONX-2", defMessageType { msgPattern = "LOGOFF", msgArgs = [] } )
  , ( "CONX-3", defMessageType { msgPattern = "HANDOVER TO $1 FROM $2", msgArgs = [ argFacility, argFacility ] } )
  , ( "CONX-4", defMessageType { msgPattern = "DATALINK $1 UP", msgArgs = [ argText ] } )
  , ( "CONX-5", defMessageType { msgPattern = "DATALINK $1 DOWN", msgArgs = [ argText ] } )
  , ( "CONX-6", defMessageType { msgPattern = "LOGON ERROR $1", msgArgs = [ argText ] } )
  ]

uplinkMessages :: Map MessageTypeID MessageType
uplinkMessages = Map.fromList
  [ ("SYSU-1", defMessageType { msgPattern = "ERROR $1", msgArgs = [ argText ] } )
  , ("SYSU-2", defMessageType { msgPattern = "NEXT DATA AUTHORITY $1", msgArgs = [ argDataAuthority ] } )
  , ("SYSU-3", defMessageType { msgPattern = "MESSAGE NOT SUPPORTED BY THIS ATC UNIT", msgArgs = [] } )
  , ("SYSU-4", defMessageType { msgPattern = "LOGICAL ACKNOWLEDGEMENT", msgArgs = [] } )
  , ("SYSU-5", defMessageType { msgPattern = "USE OF LOGICAL ACKNOWLEDGEMENT PROHIBITED", msgArgs = [] } )
  , ("SYSU-6", defMessageType { msgPattern = "LATENCY TIME VALUE $1", msgArgs = [ argLatency ] } )
  , ("SYSU-7", defMessageType { msgPattern = "MESSAGE RECEIVED TOO LATE, RESEND MESSAGE OR CONTACT BY VOICE", msgArgs = [] } )
  , ("RTEU-1", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyWU } )
  , ("RTEU-2", defMessageType { msgPattern = "PROCEED DIRECT TO $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyWU } )
  , ("RTEU-3", defMessageType { msgPattern = "AT TIME $1 PROCEED DIRECT TO $2", msgArgs = [ argTime, argNavpos ], msgReplyOpts = ReplyWU } )
  , ("RTEU-4", defMessageType { msgPattern = "AT $1 PROCEED DIRECT TO $2", msgArgs = [ argNavpos, argNavpos ], msgReplyOpts = ReplyWU } )
  , ("RTEU-5", defMessageType { msgPattern = "AT $1 PROCEED DIRECT TO $2", msgArgs = [ argFlAlt, argNavpos ], msgReplyOpts = ReplyWU } )
  , ("RTEU-6", defMessageType { msgPattern = "CLEARED TO $1 VIA $2", msgArgs = [ argNavpos, argRoute ], msgReplyOpts = ReplyWU } )
  , ("RTEU-7", defMessageType { msgPattern = "CLEARED $1", msgArgs = [ argRoute ], msgReplyOpts = ReplyWU } )
  , ("RTEU-8", defMessageType { msgPattern = "CLEARED $1", msgArgs = [ argProcedure ], msgReplyOpts = ReplyWU } )
  , ("RTEU-9", defMessageType { msgPattern = "AT $1 CLEARED $2", msgArgs = [ argNavpos, argRoute ], msgReplyOpts = ReplyWU } )
  , ("RTEU-10", defMessageType { msgPattern = "AT $1 CLEARED $2", msgArgs = [ argNavpos, argProcedure ], msgReplyOpts = ReplyWU } )
  , ("RTEU-11", defMessageType { msgPattern = "AT $1 HOLD INBOUND TRACK $2 $3 TURNS $4 LEGS", msgArgs = [ argNavpos, argDegrees, argDirection, argLegtype ], msgReplyOpts = ReplyWU } )
  , ("RTEU-12", defMessageType { msgPattern = "AT $1 HOLD AS PUBLISHED", msgArgs = [ argNavpos ], msgReplyOpts = ReplyWU } )
  , ("RTEU-13", defMessageType { msgPattern = "EXPECT FURTHER CLEARANCE AT $1", msgArgs = [ argTime ], msgReplyOpts = ReplyR } )
  , ("RTEU-14", defMessageType { msgPattern = "EXPECT $1", msgArgs = [ argClearanceType ], msgReplyOpts = ReplyR } )
  , ("RTEU-15", defMessageType { msgPattern = "CONFIRM ASSIGNED ROUTE", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("RTED-9", [])] } )
  , ("RTEU-16", defMessageType { msgPattern = "REQUEST POSITION REPORT", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("RTED-5", [])] } )
  , ("RTEU-17", defMessageType { msgPattern = "ADVISE ETA $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyY, msgReplies = [("RTED-10", [Just 1, Nothing])] } )
  , ("LATU-1", defMessageType { msgPattern = "OFFSET $1 $2 OF ROUTE", msgArgs = [ argDistance, argDirection ], msgReplyOpts = ReplyWU } )
  , ("LATU-2", defMessageType { msgPattern = "AT $1 OFFSET $2 $3 OF ROUTE", msgArgs = [ argNavpos, argDistance, argDirection ], msgReplyOpts = ReplyWU } )
  , ("LATU-3", defMessageType { msgPattern = "AT TIME $1 OFFSET $2 $3 OF ROUTE", msgArgs = [ argTime, argDistance, argDirection ], msgReplyOpts = ReplyWU } )
  , ("LATU-4", defMessageType { msgPattern = "REJOIN ROUTE", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("LATU-5", defMessageType { msgPattern = "REJOIN ROUTE BEFORE PASSING $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyWU } )
  , ("LATU-6", defMessageType { msgPattern = "REJOIN ROUTE BEFORE TIME $1", msgArgs = [ argTime ], msgReplyOpts = ReplyWU } )
  , ("LATU-7", defMessageType { msgPattern = "EXPECT BACK ON ROUTE BEFORE PASSING $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyR } )
  , ("LATU-8", defMessageType { msgPattern = "EXPECT BACK ON ROUTE BEFORE TIME $1", msgArgs = [ argTime ], msgReplyOpts = ReplyR } )
  , ("LATU-9", defMessageType { msgPattern = "RESUME OWN NAVIGATION", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("LATU-10", defMessageType { msgPattern = "CLEARED TO DEVIATE UP TO $1 OF ROUTE", msgArgs = [ argDistance ], msgReplyOpts = ReplyWU } )
  , ("LATU-11", defMessageType { msgPattern = "TURN $1 HEADING $2", msgArgs = [ argDirection, argDegrees ], msgReplyOpts = ReplyWU } )
  , ("LATU-12", defMessageType { msgPattern = "TURN $1 GROUND TRACK $2", msgArgs = [ argDirection, argDegrees ], msgReplyOpts = ReplyWU } )
  , ("LATU-13", defMessageType { msgPattern = "TURN $1 $2 DEGREES", msgArgs = [ argDirection, argDegrees ], msgReplyOpts = ReplyWU } )
  , ("LATU-14", defMessageType { msgPattern = "CONTINUE PRESENT HEADING", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("LATU-15", defMessageType { msgPattern = "AT $1 FLY HEADING $2", msgArgs = [ argNavpos, argDegrees ], msgReplyOpts = ReplyWU } )
  , ("LATU-16", defMessageType { msgPattern = "FLY HEADING $1", msgArgs = [ argDegrees ], msgReplyOpts = ReplyWU } )
  , ("LATU-17", defMessageType { msgPattern = "REPORT CLEAR OF WEATHER", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("LATU-18", defMessageType { msgPattern = "REPORT BACK ON ROUTE", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("LATU-19", defMessageType { msgPattern = "REPORT PASSING $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyWU } )
  , ("LVLU-1", defMessageType { msgPattern = "EXPECT HIGHER AT TIME $1", msgArgs = [ argTime ], msgReplyOpts = ReplyR } )
  , ("LVLU-2", defMessageType { msgPattern = "EXPECT HIGHER AT $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyR } )
  , ("LVLU-3", defMessageType { msgPattern = "EXPECT LOWER AT TIME $1", msgArgs = [ argTime ], msgReplyOpts = ReplyR } )
  , ("LVLU-4", defMessageType { msgPattern = "EXPECT LOWER AT $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyR } )
  , ("LVLU-5", defMessageType { msgPattern = "MAINTAIN $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-6", defMessageType { msgPattern = "CLIMB TO $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-7", defMessageType { msgPattern = "AT TIME $1 CLIMB TO $2", msgArgs = [ argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-8", defMessageType { msgPattern = "AT $1 CLIMB TO $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-9", defMessageType { msgPattern = "DESCEND TO $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-10", defMessageType { msgPattern = "AT TIME $1 DESCEND TO $2", msgArgs = [ argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-11", defMessageType { msgPattern = "AT $1 DESCEND TO $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-12", defMessageType { msgPattern = "CLIMB TO REACH $1 BEFORE TIME $2", msgArgs = [ argFlAlt, argTime ], msgReplyOpts = ReplyWU } )
  , ("LVLU-13", defMessageType { msgPattern = "CLIMB TO REACH $1 BEFORE PASSING $2", msgArgs = [ argFlAlt, argNavpos ], msgReplyOpts = ReplyWU } )
  , ("LVLU-14", defMessageType { msgPattern = "DESCEND TO REACH $1 BEFORE TIME $2", msgArgs = [ argFlAlt, argTime ], msgReplyOpts = ReplyWU } )
  , ("LVLU-15", defMessageType { msgPattern = "DESCEND TO REACH $1 BEFORE PASSING $2", msgArgs = [ argFlAlt, argNavpos ], msgReplyOpts = ReplyWU } )
  , ("LVLU-16", defMessageType { msgPattern = "STOP CLIMB AT $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-17", defMessageType { msgPattern = "STOP DESCENT AT $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-18", defMessageType { msgPattern = "CLIMB AT $1 OR GREATER", msgArgs = [ argVspeed ], msgReplyOpts = ReplyWU } )
  , ("LVLU-19", defMessageType { msgPattern = "CLIMB AT $1 OR LESS", msgArgs = [ argVspeed ], msgReplyOpts = ReplyWU } )
  , ("LVLU-20", defMessageType { msgPattern = "DESCEND AT $1 OR GREATER", msgArgs = [ argVspeed ], msgReplyOpts = ReplyWU } )
  , ("LVLU-21", defMessageType { msgPattern = "DESCEND AT $1 OR LESS", msgArgs = [ argVspeed ], msgReplyOpts = ReplyWU } )
  , ("LVLU-22", defMessageType { msgPattern = "EXPECT $1 $2 AFTER DEPARTURE", msgArgs = [ argFlAlt, argMinutes ], msgReplyOpts = ReplyR } )
  , ("LVLU-23", defMessageType { msgPattern = "REPORT LEAVING $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-24", defMessageType { msgPattern = "REPORT MAINTAINING $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-25", defMessageType { msgPattern = "REPORT PRESENT LEVEL", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [ ("LVLD-9", [Nothing]), ("LVLD-13", [Nothing]), ("LVLD-14", [Nothing]) ] } )
  , ("LVLU-26", defMessageType { msgPattern = "REPORT REACHING BLOCK $1 TO $2", msgArgs = [ argFlAlt, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-27", defMessageType { msgPattern = "CONFIRM ASSIGNED LEVEL", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [ ("LVLD-11", [Nothing]) ] } )
  , ("LVLU-28", defMessageType { msgPattern = "ADVISE PREFERRED LEVEL", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [ ("LVLD-12", [Nothing]) ] } )
  , ("LVLU-29", defMessageType { msgPattern = "ADVISE TOP OF DESCENT", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [ ("LVLD-18", [Nothing]) ] } )
  , ("LVLU-30", defMessageType { msgPattern = "WHEN CAN YOU ACCEPT $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyY, msgReplies = [ ("LVLD-15", [Just 1, Nothing]), ("LVLD-16", [Just 1, Nothing]), ("LVLD-17", [Just 1]) ] } )
  , ("LVLU-31", defMessageType { msgPattern = "CAN YOU ACCEPT $1 AT $2", msgArgs = [ argFlAlt, argText ], msgReplyOpts = ReplyAN } )
  , ("LVLU-32", defMessageType { msgPattern = "CAN YOU ACCEPT $1 AT TIME $2", msgArgs = [ argFlAlt, argTime ], msgReplyOpts = ReplyAN } )
  , ("CSTU-1", defMessageType { msgPattern = "CROSS $1 AT $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-2", defMessageType { msgPattern = "CROSS $1 AT OR ABOVE $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-3", defMessageType { msgPattern = "CROSS $1 AT OR BELOW $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-4", defMessageType { msgPattern = "CROSS $1 AT TIME $2", msgArgs = [ argNavpos, argTime ], msgReplyOpts = ReplyWU } )
  , ("CSTU-5", defMessageType { msgPattern = "CROSS $1 BEFORE TIME $2", msgArgs = [ argNavpos, argTime ], msgReplyOpts = ReplyWU } )
  , ("CSTU-6", defMessageType { msgPattern = "CROSS $1 AFTER TIME $2", msgArgs = [ argNavpos, argTime ], msgReplyOpts = ReplyWU } )
  , ("CSTU-7", defMessageType { msgPattern = "CROSS $1 BETWEEN TIME $2 AND TIME $3", msgArgs = [ argNavpos, argTime, argTime ], msgReplyOpts = ReplyWU } )
  , ("CSTU-8", defMessageType { msgPattern = "CROSS $1 AT $2", msgArgs = [ argNavpos, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("CSTU-9", defMessageType { msgPattern = "CROSS $1 AT $2 OR LESS", msgArgs = [ argNavpos, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("CSTU-10", defMessageType { msgPattern = "CROSS $1 AT $2 OR GREATER", msgArgs = [ argNavpos, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("CSTU-11", defMessageType { msgPattern = "CROSS $1 AT TIME $2 AT $3", msgArgs = [ argNavpos, argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-12", defMessageType { msgPattern = "CROSS $1 BEFORE TIME $2 AT $3", msgArgs = [ argNavpos, argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-13", defMessageType { msgPattern = "CROSS $1 AFTER TIME $2 AT $3", msgArgs = [ argNavpos, argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("CSTU-14", defMessageType { msgPattern = "CROSS $1 AT $2 AT $3", msgArgs = [ argNavpos, argFlAlt, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("CSTU-15", defMessageType { msgPattern = "CROSS $1 AT TIME $2 AT $3 AT $4", msgArgs = [ argNavpos, argTime, argFlAlt, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-1", defMessageType { msgPattern = "EXPECT SPEED CHANGE AT TIME $1", msgArgs = [ argTime ], msgReplyOpts = ReplyR } )
  , ("SPDU-2", defMessageType { msgPattern = "EXPECT SPEED CHANGE AT $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyR } )
  , ("SPDU-3", defMessageType { msgPattern = "EXPECT SPEED CHANGE AT $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyR } )
  , ("SPDU-4", defMessageType { msgPattern = "MAINTAIN $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-5", defMessageType { msgPattern = "MAINTAIN PRESENT SPEED", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("SPDU-6", defMessageType { msgPattern = "MAINTAIN $1 OR GREATER", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-7", defMessageType { msgPattern = "MAINTAIN $1 OR LESS", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-8", defMessageType { msgPattern = "MAINTAIN $1 TO $2", msgArgs = [ argSpeed, argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-9", defMessageType { msgPattern = "INCREASE SPEED TO $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-10", defMessageType { msgPattern = "INCREASE SPEED TO $1 OR GREATER", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-11", defMessageType { msgPattern = "REDUCE SPEED TO $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-12", defMessageType { msgPattern = "REDUCE SPEED TO $1 OR LESS", msgArgs = [ argSpeed ], msgReplyOpts = ReplyWU } )
  , ("SPDU-13", defMessageType { msgPattern = "RESUME NORMAL SPEED", msgArgs = [], msgReplyOpts = ReplyWU } )
  , ("SPDU-14", defMessageType { msgPattern = "NO SPEED RESTRICTION", msgArgs = [], msgReplyOpts = ReplyR } )
  , ("SPDU-15", defMessageType { msgPattern = "REPORT $1 SPEED", msgArgs = [ argSpeedType ], msgReplyOpts = ReplyY, msgReplies = [ ("SPDD-3", [Just 1, Nothing]) ] } )
  , ("SPDU-16", defMessageType { msgPattern = "CONFIRM ASSIGNED SPEED", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("SPDD-4", [])] } )
  , ("SPDU-17", defMessageType { msgPattern = "WHEN CAN YOU ACCEPT $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyY, msgReplies = [ ("SPDD-5", [Just 1, Nothing]), ("SPDD-6", [Just 1]) ] } )
  , ("ADVU-1", defMessageType { msgPattern = "$1 ALTIMETER $2", msgArgs = [ argFacility, argAltimeter ], msgReplyOpts = ReplyR })
  , ("ADVU-2", defMessageType { msgPattern = "SERVICE TERMINATED", msgArgs = [], msgReplyOpts = ReplyR })
  , ("ADVU-3", defMessageType { msgPattern = "IDENTIFIED $1", msgArgs = [ argText ], msgReplyOpts = ReplyR })
  , ("ADVU-4", defMessageType { msgPattern = "IDENTIFICATION LOST", msgArgs = [], msgReplyOpts = ReplyR })
  , ("ADVU-5", defMessageType { msgPattern = "ATIS $1", msgArgs = [ argAtisCode ], msgReplyOpts = ReplyR })
  , ("ADVU-6", defMessageType { msgPattern = "REQUEST AGAIN WITH NEXT ATC UNIT", msgArgs = [] })
  , ("ADVU-7", defMessageType { msgPattern = "TRAFFIC IS $1", msgArgs = [ argText ], msgReplyOpts = ReplyR })
  , ("ADVU-8", defMessageType { msgPattern = "REPORT SIGHTING AND PASSING OPPOSITE DIRECTION $1", msgArgs = [ argText ], msgReplyOpts = ReplyWU })
  , ("ADVU-9", defMessageType { msgPattern = "SQUAWK $1", msgArgs = [ argXpdr ], msgReplyOpts = ReplyWU })
  , ("ADVU-10", defMessageType { msgPattern = "STOP SQUAWK", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-11", defMessageType { msgPattern = "STOP ADS-B TRANSMISSION", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-12", defMessageType { msgPattern = "SQUAWK MODE C", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-13", defMessageType { msgPattern = "STOP SQUAWK MODE C", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-14", defMessageType { msgPattern = "CONFIRM SQUAWK CODE", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("ADVD-1", [])] })
  , ("ADVU-15", defMessageType { msgPattern = "SQUAWK IDENT", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-16", defMessageType { msgPattern = "ACTIVATE ADS-C", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-17", defMessageType { msgPattern = "ADS-C OUT OF SERVICE REVERT TO VOICE POSITION REPORTS", msgArgs = [], msgReplyOpts = ReplyWU })
  , ("ADVU-18", defMessageType { msgPattern = "RELAY TO $1", msgArgs = [ argCallsign ], msgReplyOpts = ReplyWU } )
  , ("ADVU-19", defMessageType { msgPattern = "$1 DEVIATION DETECTED. VERIFY AND ADVISE", msgArgs = [ argDeviationType ], msgReplyOpts = ReplyWU })
  , ("COMU-1", defMessageType { msgPattern = "CONTACT $1 $2 $3", msgArgs = [ optArg argCallsign, argFreq, optArg argCallsign ], msgReplyOpts = ReplyWU } )
  , ("COMU-2", defMessageType { msgPattern = "AT $1 CONTACT $2 $3 $4", msgArgs = [ argNavpos, optArg argCallsign, argFreq, optArg argCallsign ], msgReplyOpts = ReplyWU } )
  , ("COMU-3", defMessageType { msgPattern = "AT TIME $1 CONTACT $2 $3", msgArgs = [ argTime, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-4", defMessageType { msgPattern = "SECONDARY FREQUENCY $1", msgArgs = [ argFreq ], msgReplyOpts = ReplyR } )
  , ("COMU-5", defMessageType { msgPattern = "MONITOR $1 $2", msgArgs = [ argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-6", defMessageType { msgPattern = "AT $1 MONITOR $2 $3", msgArgs = [ argNavpos, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-7", defMessageType { msgPattern = "AT TIME $1 MONITOR $2 $3", msgArgs = [ argTime, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-8", defMessageType { msgPattern = "CHECK STUCK MICROPHONE $1", msgArgs = [ argFreq ] } )
  , ("COMU-9", defMessageType { msgPattern = "CURRENT ATC UNIT $1", msgArgs = [ argText ] } )
  , ("EMGU-1", defMessageType { msgPattern = "REPORT ENDURANCE AND PERSONS ON BOARD", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("EMGD-3", [])] } )
  , ("EMGU-2", defMessageType { msgPattern = "IMMEDIATELY", msgArgs = [], msgReplyOpts = ReplyY } )
  , ("EMGU-3", defMessageType { msgPattern = "CONFIRM ADS-C EMERGENCY", msgArgs = [], msgReplyOpts = ReplyAN } )
  , ("SUPU-1", defMessageType { msgPattern = "WHEN READY", msgArgs = [] } )
  , ("SUPU-2", defMessageType { msgPattern = "DUE TO $1", msgArgs = [ argReason ] } )
  , ("SUPU-3", defMessageType { msgPattern = "EXPEDITE", msgArgs = [] } )
  , ("SUPU-4", defMessageType { msgPattern = "REVISED $1", msgArgs = [ argReason ] } )
  , ("RSPU-1", defMessageType { msgPattern = "UNABLE", msgArgs = [] } )
  , ("RSPU-2", defMessageType { msgPattern = "STANDBY", msgArgs = [] } )
  , ("RSPU-3", defMessageType { msgPattern = "REQUEST DEFERRED", msgArgs = [] } )
  , ("RSPU-4", defMessageType { msgPattern = "ROGER", msgArgs = [] } )
  , ("RSPU-5", defMessageType { msgPattern = "AFFIRM", msgArgs = [] } )
  , ("RSPU-6", defMessageType { msgPattern = "NEGATIVE", msgArgs = [] } )
  , ("RSPU-7", defMessageType { msgPattern = "REQUEST FORWARDED", msgArgs = [] } )
  , ("RSPU-8", defMessageType { msgPattern = "CONFIRM REQUEST", msgArgs = [] } )
  , ("TXTU-1", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyR } )
  , ("TXTU-2", defMessageType { msgPattern = "$1", msgArgs = [ argText ] } )
  , ("TXTU-3", defMessageType { msgPattern = "$1", msgArgs = [ argText ] } )
  , ("TXTU-4", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyWU } )
  , ("TXTU-5", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyAN } )
  ]

-- buildParseTrie :: [(MessageTypeID, MessagePattern)] -> Trie MessagePatternItem MessageTypeID

-- -- -- messages from aircraft to ATC --
-- var downlink_messages = {
--     "SYSD-1": { txt: "ERROR $1", args: [ARG_TEXT], r_opts: [] },
--     "SYSD-2": { txt: "LOGICAL ACKNOWLEDGEMENT", args: [], r_opts: [] },
--     "SYSD-3": { txt: "NOT CURRENT DATA AUTHORITY", args: [], r_opts: [] },
--     "SYSD-4": { txt: "CURRENT DATA AUTHORITY", args: [], r_opts: [] },
--     "SYSD-5": { txt: "NOT AUTHORIZED NEXT DATA AUTHORITY $1 $2", args: [ARG_DATA_AUTHORITY, ARG_DATA_AUTHORITY], r_opts: [] },
--     "SYSD-6": { txt: "MESSAGE RECEIVED TOO LATE, RESEND MESSAGE OR CONTACT BY VOICE", args: [], r_opts: [] },
--     "SYSD-7": { txt: "AIRCRAFT CPDLC INHIBITED", args: [], r_opts: [] },
-- 
--     "RTED-1": { txt: "REQUEST DIRECT TO $1", args: [ARG_NAVPOS], r_opts: ["y"] },
--     "RTED-2": { txt: "REQUEST $1", args: [ARG_TEXT], r_opts: ["y"] },
--     "RTED-3": { txt: "REQUEST CLEARANCE $1", args: [ARG_ROUTE], r_opts: ["y"] },
--     "RTED-4": { txt: "REQUEST $1 CLEARANCE", args: [ARG_CLEARANCE_TYPE], r_opts: ["y"] },
--     "RTED-5": { txt: "POSITION REPORT $1", args: [ARG_POSREP], r_opts: ["y"] },
--     "RTED-6": { txt: "REQUEST HEADING $1", args: [ARG_DEGREES], r_opts: ["y"] },
--     "RTED-7": { txt: "REQUEST GROUND TRACK $1", args: [ARG_DEGREES], r_opts: ["y"] },
--     "RTED-8": { txt: "WHEN CAN WE EXPECT BACK ON ROUTE", args: [], r_opts: ["y"] },
--     "RTED-9": { txt: "ASSIGNED ROUTE $1", args: [ARG_ROUTE], r_opts: [] },
--     "RTED-10": { txt: "ETA $1 TIME $2", args: [ARG_NAVPOS, ARG_TIME], r_opts: [] },
-- 
--     "LATD-1": { txt: "REQUEST OFFSET $1 $2 OF ROUTE", args: [ARG_DISTANCE, ARG_DIRECTION], r_opts: ["y"] },
--     "LATD-2": { txt: "REQUEST WEATHER DEVIATION UP TO $1 OF ROUTE", args: [ARG_DISTANCE], r_opts: ["y"] },
--     "LATD-3": { txt: "CLEAR OF WEATHER", args: [], r_opts: [] },
--     "LATD-4": { txt: "BACK ON ROUTE", args: [], r_opts: [] },
--     "LATD-5": { txt: "DIVERTING TO $1 VIA $2", args: [ARG_NAVPOS, ARG_ROUTE], r_opts: ["y"] },
--     "LATD-6": { txt: "OFFSETTING $1 $2 OF ROUTE", args: [ARG_DISTANCE, ARG_DIRECTION], r_opts: ["y"] },
--     "LATD-7": { txt: "DEVIATING $1 $2 OF ROUTE", args: [ARG_DISTANCE, ARG_DIRECTION], r_opts: ["y"] },
--     "LATD-8": { txt: "PASSING $1", args: [ARG_NAVPOS], r_opts: [] },
-- 
--     "LVLD-1": { txt: "REQUEST LEVEL $1", args: [ARG_FL_ALT], r_opts: ["y"] },
--     "LVLD-2": { txt: "REQUEST CLIMB TO $1", args: [ARG_FL_ALT], r_opts: ["y"] },
--     "LVLD-3": { txt: "REQUEST DESCENT TO $1", args: [ARG_FL_ALT], r_opts: ["y"] },
--     "LVLD-4": { txt: "AT $1 REQUEST $2", args: [ARG_NAVPOS, ARG_FL_ALT], r_opts: ["y"] },
--     "LVLD-5": { txt: "AT TIME $1 REQUEST $2", args: [ARG_TIME, ARG_FL_ALT], r_opts: ["y"] },
--     "LVLD-6": { txt: "WHEN CAN WE EXPECT LOWER LEVEL", args: [], r_opts: ["y"] },
--     "LVLD-7": { txt: "WHEN CAN WE EXPECT HIGHER LEVEL", args: [], r_opts: ["y"] },
--     "LVLD-8": { txt: "LEAVING LEVEL $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-9": { txt: "MAINTAINING LEVEL $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-10": { txt: "REACHING BLOCK $1 TO $2", args: [ARG_FL_ALT, ARG_FL_ALT], r_opts: [] },
--     "LVLD-11": { txt: "ASSIGNED LEVEL $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-12": { txt: "PREFERRED LEVEL $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-13": { txt: "CLIMBING TO $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-14": { txt: "DESCENDING TO $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-15": { txt: "WE CAN ACCEPT $1 AT TIME $2", args: [ARG_FL_ALT, ARG_TIME], r_opts: [] },
--     "LVLD-16": { txt: "WE CAN ACCEPT $1 AT $2", args: [ARG_FL_ALT, ARG_NAVPOS], r_opts: [] },
--     "LVLD-17": { txt: "WE CANNOT ACCEPT $1", args: [ARG_FL_ALT], r_opts: [] },
--     "LVLD-18": { txt: "TOP OF DESCENT $1 TIME $2", args: [ARG_TEXT, ARG_TIME], r_opts: [] },
-- 
--     "ADVD-1": { txt: "SQUAWKING $1", args: [ARG_XPDR], r_opts: [] },
--     "ADVD-2": { txt: "TRAFFIC $1", args: [ARG_TEXT], r_opts: [] },
-- 
--     "SPDD-1": { txt: "REQUEST $1", args: [ARG_SPEED], r_opts: ["y"] },
--     "SPDD-2": { txt: "WHEN CAN WE EXPECT $1", args: [ARG_SPEED], r_opts: ["y"] },
--     "SPDD-3": { txt: "$1 SPEED $2", args: [ARG_SPEED_TYPE, ARG_SPEED], r_opts: [] },
--     "SPDD-4": { txt: "ASSIGNED SPEED $1", args: [ARG_SPEED], r_opts: [] },
--     "SPDD-5": { txt: "WE CAN ACCEPT $1 AT TIME $2", args: [ARG_SPEED, ARG_TIME], r_opts: [] },
--     "SPDD-6": { txt: "WE CANNOT ACCEPT $1", args: [ARG_SPEED], r_opts: [] },
-- 
--     "RSPD-1": { txt: "WILCO", args: [], r_opts: [] }, 
--     "RSPD-2": { txt: "UNABLE", args: [], r_opts: [] }, 
--     "RSPD-3": { txt: "STANDBY", args: [], r_opts: [] }, 
--     "RSPD-4": { txt: "ROGER", args: [], r_opts: [] }, 
--     "RSPD-5": { txt: "AFFIRM", args: [], r_opts: [] }, 
--     "RSPD-6": { txt: "NEGATIVE", args: [], r_opts: [] },    
--     
--     "COMD-1": { txt: "REQUEST VOICE CONTACT $1", args: [ARG_FREQ], r_opts: ["y"] }, 
--     "COMD-2": { txt: "RELAY FROM $1", args: [ARG_TEXT], r_opts: ["n"] }, 
-- 
--     "EMGD-1": { txt: "PAN PAN PAN", args: [], r_opts: ["y"] }, 
--     "EMGD-2": { txt: "MAYDAY MAYDAY MAYDAY", args: [], r_opts: ["y"] }, 
--     "EMGD-3": { txt: "$1 ENDURANCE AND $2 POB", args: [ARG_ENDURANCE, ARG_INTEGER], r_opts: ["y"] }, 
--     "EMGD-4": { txt: "CANCEL EMERGENCY", args: [], r_opts: ["y"] }, 
-- 
--     "SUPD-1": { txt: "DUE TO $1", args: [ARG_REASON], r_opts: [] },
-- 
--     "TXTD-1": { txt: "$1", args: [ARG_TEXT], r_opts: ["y"] },
--     "TXTD-2": { txt: "$1", args: [ARG_TEXT], r_opts: [] },
-- };
-- 
-- var formatMessagePart = func (type, args) {
--     if (args == nil) args = [];
--     var messageType =
--             contains(pseudo_messages, type) ? pseudo_messages[type] :
--             contains(uplink_messages, type) ? uplink_messages[type] :
--             contains(downlink_messages, type) ? downlink_messages[type] :
--             nil;
--     if (messageType == nil) {
--         return '[' ~ type ~ '] ' ~ string.join(' ', args);
--     }
--     var txt = messageType.txt;
--     for (var i = 0; i < size(args); i += 1) {
--         txt = string.replace(txt, '$' ~ (i + 1), args[i]);
--     }
--     # debug.dump("FORMAT", type, args, messageType, txt);
--     return txt;
-- };
-- 
-- var formatMessage = func (parts) {
--     var formattedParts = [];
--     foreach (var part; parts) {
--         append(formattedParts, formatMessagePart(part.type, part.args));
--     }
--     return string.join(' ', formattedParts);
-- };
-- 
-- var formatMessagePartFancy = func (type, args) {
--     if (args == nil) args = [];
--     var messageType =
--             contains(pseudo_messages, type) ? pseudo_messages[type] :
--             contains(uplink_messages, type) ? uplink_messages[type] :
--             contains(downlink_messages, type) ? downlink_messages[type] :
--             nil;
--     if (messageType == nil) {
--         logprint(4, 'INVALID MESSAGE', type, debug.string(args));
--         return [];
--     }
--     var words = split(' ', messageType.txt);
--     if (substr(type, 0, 3) == 'TXT') {
--         words = ['FREE', 'TEXT'] ~ words;
--     }
--     var line = [];
--     var elems = [];
--     foreach (var word; words) {
--         if (substr(word, 0, 1) == '$') {
--             if (size(line) > 0) {
--                 append(elems, { type: 0, value: string.join(' ', line) });
--                 line = [];
--             }
--             var i = int(substr(word, 1)) - 1;
--             var value = args[i];
--             if (value == nil or value == '') {
--                 value = '----------------';
--             }
--             append(elems, { type: messageType.args[i], value: value });
--         }
--         else {
--             append(line, word);
--         }
--     }
--     if (size(line) > 0) {
--         append(elems, { type: 0, value: string.join(' ', line) });
--         line = [];
--     }
--     return elems;
-- };
-- 
-- var formatMessageFancy = func (parts) {
--     var formattedParts = [];
--     foreach (var part; parts) {
--         append(formattedParts, formatMessagePartFancy(part.type, part.args));
--     }
--     return formattedParts;
-- };
-- 
-- var messageRA = func (type) {
--     var messageType =
--             contains(uplink_messages, type) ? uplink_messages[type] :
--             contains(downlink_messages, type) ? downlink_messages[type] :
--             nil;
--     if (messageType == nil) return '';
--     return string.uc(string.join('', messageType.r_opts));
-- };
-- 
-- var messageFromNode = func (node) {
--     var msg = node.getValues();
--     if (typeof(msg.parts) != 'vector') msg.parts = [msg.parts];
--     foreach (var part; msg.parts) {
--         if (typeof(part.args) != 'vector') part.args = [part.args];
--     }
--     return msg;
-- };
