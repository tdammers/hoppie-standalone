{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.CPDLC.MessageTypes
where

import Web.Hoppie.Telex

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.Void
import Data.String
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P (decimal)
import Data.List
import Data.Maybe
import Data.Char

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

argSpecToParser :: ArgSpec -> P.Parsec Void ByteString (Maybe ByteString)
argSpecToParser (ArgSpec ty True) = P.optional (argTyToParser ty)
argSpecToParser (ArgSpec ty False) = Just <$> argTyToParser ty

altUnitP :: P.Parsec Void ByteString ByteString
altUnitP =
  P.choice . map P.string $
    [ "FT"
    , "FEET"
    , "M"
    , "METERS"
    ]

speedUnitP :: P.Parsec Void ByteString ByteString
speedUnitP =
  P.choice . map P.string $
    [ "KT"
    , "KTS"
    , "KNOTS"
    , "MPH"
    , "KMH"
    , "KPH"
    ]

vspeedUnitP :: P.Parsec Void ByteString ByteString
vspeedUnitP =
  P.choice . map P.string $
    [ "FPM"
    , "FTMIN"
    , "MPS"
    , "MSEC"
    ]

distanceUnitP :: P.Parsec Void ByteString ByteString
distanceUnitP =
  P.choice . map P.string $
    [ "M"
    , "NM"
    , "NMI"
    , "KM"
    , "MILES"
    ]

altimeterUnitP :: P.Parsec Void ByteString ByteString
altimeterUnitP =
  P.choice . map P.string $
    [ "HPA"
    , "HECTOPASCAL"
    , "HECTOPASCALS"
    , "INHG"
    , "MMHG"
    ]

argTyToParser :: ArgTy -> P.Parsec Void ByteString ByteString
argTyToParser ArgFlAlt =
  P.choice
    [ P.try $ rawAltitudeP
    , P.try $ mconcat <$> sequence [ P.string "FL", "" <$ P.space, integralP ]
    , P.try $ mconcat <$> sequence [ rawIntegralP, "" <$ P.space, altUnitP ]
    ]
argTyToParser ArgSpeed =
  P.choice
    [ P.try $ mconcat <$> sequence [ P.string "M", "" <$ P.space, decimalP ]
    , P.try $ mconcat <$> sequence [ P.string "M", "." <$ P.space, integralP ]
    , P.try $ mconcat <$> sequence [ integralP, "" <$ P.space, speedUnitP ]
    ]
argTyToParser ArgNavpos = nonKeywordP
argTyToParser ArgRoute = routeP
argTyToParser ArgXpdr = integralP
argTyToParser ArgCallsign = anyBarewordsP
argTyToParser ArgFreq = decimalP
argTyToParser ArgTime = decimalP
-- argTyToParser ArgDirection 
-- argTyToParser ArgDegrees
-- argTyToParser ArgAtisCode
-- argTyToParser ArgDeviationType
-- argTyToParser ArgEndurance
-- argTyToParser ArgLegtype
argTyToParser ArgText = P.takeWhile1P Nothing (const True)
argTyToParser ArgInteger = integralP
-- argTyToParser ArgReason
-- argTyToParser ArgClearanceType
-- argTyToParser ArgPosrep
argTyToParser ArgDistance =
  P.try $ mconcat <$> sequence [ integralP, "" <$ P.space, distanceUnitP ]
argTyToParser ArgProcedure = nonKeywordP
-- argTyToParser ArgSpeedType
argTyToParser ArgFacility = anyBarewordsP
argTyToParser ArgAltimeter =
  P.choice
    [ P.try $ mconcat <$> sequence [ decimalP, "" <$ P.space, altimeterUnitP ]
    , P.try $ decimalP
    ]
-- argTyToParser ArgDataAuthority
argTyToParser ArgVspeed =
    P.try $ mconcat <$> sequence [ decimalP, "" <$ P.space, vspeedUnitP ]
-- argTyToParser ArgMinutes
-- argTyToParser ArgLatency
argTyToParser _ = P.takeWhile1P Nothing (const True)

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
  | MessageArg a
  | MessageOptional Bool [ByteString]
  deriving (Show, Read, Eq, Ord, Functor)

messagePatternP :: P.Parsec Void ByteString (MessagePattern ArgRef)
messagePatternP = MessagePattern <$> P.many messagePatternItemP

messagePatternItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
messagePatternItemP =
  (optItemP <|> argRefItemP <|> litItemP) <* P.space

argRefItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
argRefItemP = do
  void $ P.char (ord8 '$')
  MessageArg <$> P.decimal

litItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
litItemP =
  MessageLiteral <$>
    P.takeWhile1P
      (Just "characters")
      (\c -> c /= ord8 '$' && not (isSpace8 c))

bracedP :: P.Parsec Void ByteString a -> P.Parsec Void ByteString a
bracedP x = openBraceP *> x <* closeBraceP

openBraceP :: P.Parsec Void ByteString ()
openBraceP = void $ P.char (ord8 '{') <* P.space

closeBraceP :: P.Parsec Void ByteString ()
closeBraceP = void $ P.char (ord8 '}') <* P.space


optItemP :: P.Parsec Void ByteString (MessagePatternItem ArgRef)
optItemP =
  bracedP $ do
    defaultChar <- P.optional $ P.char (ord8 '!')
    MessageOptional (isNothing defaultChar) <$>
      P.many (P.takeWhile1P Nothing (\c -> not (isSpace8 c || c == ord8 '}')) <* P.space)

instance IsString (MessagePattern ArgRef) where
  fromString = either (error . P.errorBundlePretty) id . P.runParser messagePatternP "input" . BS8.pack

type MessageTypeID = ByteString

data MessageType =
  MessageType
    { msgPattern :: MessagePattern ArgRef
    , msgArgs :: [ArgSpec]
    , msgReplyOpts :: ReplyOpts
    , msgReplies :: [(MessageTypeID, [Maybe ArgRef])]
    , msgSuggestedSupp :: [MessageTypeID]
    }
    deriving (Show, Read)

defMessageType :: MessageType
defMessageType =
  MessageType
    { msgPattern = MessagePattern []
    , msgArgs = []
    , msgReplyOpts = ReplyN
    , msgReplies = []
    , msgSuggestedSupp = []
    }

resolveMessagePatternWith :: (ArgRef -> a) -> MessagePattern ArgRef -> MessagePattern a
resolveMessagePatternWith f = MessagePattern . map (resolveMessagePatternItemWith f) . messagePatternItems

resolveMessagePatternArgs :: [a] -> MessagePattern ArgRef -> MessagePattern a
resolveMessagePatternArgs specs =
  resolveMessagePatternWith
    (\i -> specs !! (fromIntegral i - 1))

resolveMessagePatternItemWith :: (ArgRef -> a) -> MessagePatternItem ArgRef -> MessagePatternItem a
resolveMessagePatternItemWith _ (MessageLiteral str) = MessageLiteral str
resolveMessagePatternItemWith _ (MessageOptional def strs) = MessageOptional def strs
resolveMessagePatternItemWith f (MessageArg i) = MessageArg $ f i

matchMessage :: MessageType -> ByteString -> ReplyOpts -> Maybe [ByteString]
matchMessage msgTy src ropts
  | msgReplyOpts msgTy /= ropts
  = Nothing
  | otherwise
  = P.parseMaybe
        (messagePatternToParser (resolveMessagePatternArgs (msgArgs msgTy) $ msgPattern msgTy))
        (scrub src)

parseMessage :: Maybe ReplyOpts -> Map MessageTypeID MessageType -> ByteString -> [[(MessageTypeID, [ByteString])]]
parseMessage raMay msgTypes = go . scrub
  where
    go remainder = do
      parser <- parsers
      (msgID, args, remainder') <- maybeToList $ P.parseMaybe parser remainder
      case remainder' of
        "" -> return [(msgID, args)]
        _ -> ((msgID, args) :) <$> go remainder'

    orderBySpecificity = sortOn (negate . messageTypeSpecificity . snd)

    filterByRA Nothing = id
    filterByRA (Just ra) = filter ((== ra) . msgReplyOpts . snd)

    parsers :: [P.Parsec Void ByteString (MessageTypeID, [ByteString], ByteString)]
    parsers = flip map (orderBySpecificity . filterByRA raMay $ Map.toList msgTypes) $ \(msgID, msg) -> do
                args <- messagePatternToParser
                          (resolveMessagePatternArgs (msgArgs msg) (msgPattern msg))
                void P.space
                remainder <- P.takeWhileP Nothing (const True)
                return (msgID, args, remainder)

renderMessage :: Map MessageTypeID MessageType -> MessageTypeID -> [ByteString] -> ByteString
renderMessage msgTypes msgID args =
  let msg = fromMaybe (error $ "Message type does not exist: " ++ show msgID) $ Map.lookup msgID msgTypes
      pattern = resolveMessagePatternWith
                  (\w ->
                    let i = fromIntegral w - 1
                    in ( (args ++ repeat "*ERROR*") !! i
                       , argTy (msgArgs msg !! i)
                       )
                  )
                  (msgPattern msg)
  in
    mconcat . intersperse " " . concatMap renderItem . messagePatternItems $ pattern
  where
    renderItem :: MessagePatternItem (ByteString, ArgTy) -> [ByteString]
    renderItem (MessageLiteral lit) = [lit]
    renderItem (MessageOptional True _) = []
    renderItem (MessageOptional False lits) = lits
    renderItem (MessageArg ("", _)) = []
    renderItem (MessageArg (val, _ty)) = [val]

scrub :: ByteString -> ByteString
scrub =
  normalizeWhitespace .
  telexFilter .
  removeAt .
  removeAtUnderscoreAt
  where
    normalizeWhitespace = BS8.unwords . BS8.words
    removeAt = BS.map (ord8 . \case { '@' -> ' '; c -> c } . chr8)
    removeAtUnderscoreAt str =
      case BS.breakSubstring "@_@" str of
        (a, "") -> a
        (a, b) -> a <> " " <> removeAtUnderscoreAt (BS.drop 3 b)


routeP :: P.Parsec Void ByteString ByteString
routeP =
  P.try spacedRouteP <|> P.try dottedRouteP

spacedRouteP :: P.Parsec Void ByteString ByteString
spacedRouteP =
  mconcat . intersperse " " <$> P.some nonKeywordP

dottedRouteP :: P.Parsec Void ByteString ByteString
dottedRouteP =
  mconcat . intersperse " " <$> (routeItemP `P.sepBy` P.char (ord8 '.')) <* (P.space1 <|> P.eof)
  where
    routeItemP :: P.Parsec Void ByteString ByteString
    routeItemP = do
      f <- P.satisfy (isAlpha . chr8)
      r <- P.takeWhileP Nothing (isAlphaNum . chr8)
      let bw = BS.cons f r
      if bw `elem` keywords then
        fail "Expected non-keyword"
      else
        return bw

barewordP :: ByteString -> P.Parsec Void ByteString ByteString
barewordP str =
  P.string str <* (P.space1 <|> P.eof)

nonKeywordP :: P.Parsec Void ByteString ByteString
nonKeywordP = P.try $ do
  bw <- anyBarewordP
  if bw `elem` keywords then
    fail "Expected non-keyword"
  else
    return bw

keywords :: [ByteString]
keywords =
  [ "CLIMB"
  , "DESCEND"
  , "TO"
  , "VIA"
  , "SQUAWK"
  , "DUE"
  , "DIRECT"
  , "LOGON"
  ]

anyBarewordP :: P.Parsec Void ByteString ByteString
anyBarewordP = do
  f <- P.satisfy (isAlpha . chr8)
  r <- P.takeWhileP (Just "non-space") (not . isSpace8) <* (P.space1 <|> P.eof)
  return $ BS.cons f r

anyBarewordsP :: P.Parsec Void ByteString ByteString
anyBarewordsP =
  mconcat . intersperse " " <$> P.some anyBarewordP

squawkP :: P.Parsec Void ByteString ByteString
squawkP =
  BS.pack <$> replicateM 4 (P.satisfy (`elem` [ord8 '0' .. ord8 '7']))

rawAltitudeP :: P.Parsec Void ByteString ByteString
rawAltitudeP = do
  alt <- integralP
  if "000" `BS.isSuffixOf` alt then
    return alt
  else
    fail "Expected altitude"

integralP :: P.Parsec Void ByteString ByteString
integralP =
  rawIntegralP <* (P.space1 <|> P.eof)

rawIntegralP :: P.Parsec Void ByteString ByteString
rawIntegralP =
  P.takeWhile1P (Just "digits") isNum8

decimalP :: P.Parsec Void ByteString ByteString
decimalP = P.try $ do
  intPart <- P.takeWhileP (Just "intPart") isNum8
  dot <- P.string "."
  fracPart <- P.takeWhileP (Just "fracPart") isNum8
  P.space
  if intPart == "" && fracPart == "" then
    fail $ "Need at least one of intPart, fracPart"
  else
    return $ intPart <> dot <> fracPart

messagePatternItemToParser :: MessagePatternItem ArgSpec -> P.Parsec Void ByteString [ByteString]
messagePatternItemToParser (MessageLiteral lit) =
  [] <$ barewordP lit
messagePatternItemToParser (MessageOptional _ lits) =
  [] <$ P.optional (P.try $ mapM_ barewordP lits)
messagePatternItemToParser (MessageArg argSpec) =
  (:[]) . fromMaybe "" <$> argSpecToParser argSpec

messagePatternToParser :: MessagePattern ArgSpec -> P.Parsec Void ByteString [ByteString]
messagePatternToParser (MessagePattern items) =
  mconcat <$> mapM messagePatternItemToParser items

messageTypeSpecificity :: MessageType -> Int
messageTypeSpecificity msg =
  100 * leadingLiterals + firstArgSpecificity
  where
    leadingLiterals = length . takeWhile isLiteral . messagePatternItems . msgPattern $ msg
    firstArgSpecificity = case take 1 . dropWhile isLiteral . messagePatternItems . msgPattern $ msg of
      (MessageArg i:_) ->
        case argTy (msgArgs msg !! fromIntegral (i - 1)) of
          ArgText -> 1
          ArgRoute -> 2
          _ -> 10
      _ -> 0
              

isLiteral :: MessagePatternItem a -> Bool
isLiteral MessageLiteral {} = True
isLiteral MessageOptional {} = True
isLiteral _ = False

allMessageTypes :: Map MessageTypeID MessageType
allMessageTypes =
  pseudoMessages <> uplinkMessages <> downlinkMessages

sendableMessageTypes :: Map MessageTypeID MessageType
sendableMessageTypes =
  uplinkMessages <> downlinkMessages

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
  -- , ("RTEU-1", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyWU } )
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
  , ("LVLU-6", defMessageType { msgPattern = "CLIMB TO {AND MAINTAIN} $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-7", defMessageType { msgPattern = "AT TIME $1 CLIMB TO {AND MAINTAIN} $2", msgArgs = [ argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-8", defMessageType { msgPattern = "AT $1 CLIMB TO {AND MAINTAIN} $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-9", defMessageType { msgPattern = "DESCEND TO {AND MAINTAIN} $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-10", defMessageType { msgPattern = "AT TIME $1 DESCEND TO {AND MAINTAIN} $2", msgArgs = [ argTime, argFlAlt ], msgReplyOpts = ReplyWU } )
  , ("LVLU-11", defMessageType { msgPattern = "AT $1 DESCEND TO {AND MAINTAIN} $2", msgArgs = [ argNavpos, argFlAlt ], msgReplyOpts = ReplyWU } )
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
  , ("ADVU-6", defMessageType { msgPattern = "REQUEST AGAIN WITH NEXT {ATC} UNIT", msgArgs = [] })
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
  , ("COMU-1", defMessageType { msgPattern = "CONTACT $1 $2 $3", msgArgs = [ optArg argCallsign, argFreq, optArg argFacility ], msgReplyOpts = ReplyWU } )
  , ("COMU-2", defMessageType { msgPattern = "AT $1 CONTACT $2 $3 $4", msgArgs = [ argNavpos, optArg argCallsign, argFreq, optArg argFacility ], msgReplyOpts = ReplyWU } )
  , ("COMU-3", defMessageType { msgPattern = "AT TIME $1 CONTACT $2 $3", msgArgs = [ argTime, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-4", defMessageType { msgPattern = "SECONDARY FREQUENCY $1", msgArgs = [ argFreq ], msgReplyOpts = ReplyR } )
  , ("COMU-5", defMessageType { msgPattern = "MONITOR $1 $2", msgArgs = [ argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-6", defMessageType { msgPattern = "AT $1 MONITOR $2 $3", msgArgs = [ argNavpos, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-7", defMessageType { msgPattern = "AT TIME $1 MONITOR $2 $3", msgArgs = [ argTime, argCallsign, argFreq ], msgReplyOpts = ReplyWU } )
  , ("COMU-8", defMessageType { msgPattern = "CHECK STUCK MICROPHONE $1", msgArgs = [ argFreq ] } )
  , ("COMU-9", defMessageType { msgPattern = "CURRENT ATC UNIT $1", msgArgs = [ argFacility ] } )
  , ("EMGU-1", defMessageType { msgPattern = "REPORT ENDURANCE AND PERSONS ON BOARD", msgArgs = [], msgReplyOpts = ReplyY, msgReplies = [("EMGD-3", [])] } )
  , ("EMGU-2", defMessageType { msgPattern = "IMMEDIATELY", msgArgs = [], msgReplyOpts = ReplyY } )
  , ("EMGU-3", defMessageType { msgPattern = "CONFIRM ADS-C EMERGENCY", msgArgs = [], msgReplyOpts = ReplyAN } )
  , ("SUPU-1", defMessageType { msgPattern = "WHEN READY", msgArgs = [] } )
  , ("SUPU-2", defMessageType { msgPattern = "DUE {!TO} $1", msgArgs = [ argReason ] } )
  , ("SUPU-3", defMessageType { msgPattern = "EXPEDITE", msgArgs = [] } )
  , ("SUPU-4", defMessageType { msgPattern = "REVISED $1", msgArgs = [ argReason ] } )
  , ("RSPU-1", defMessageType { msgPattern = "UNABLE", msgArgs = [], msgSuggestedSupp = ["SUPU-2"] } )
  , ("RSPU-2", defMessageType { msgPattern = "STANDBY", msgArgs = [] } )
  , ("RSPU-3", defMessageType { msgPattern = "REQUEST DEFERRED", msgArgs = [] } )
  , ("RSPU-4", defMessageType { msgPattern = "ROGER", msgArgs = [] } )
  , ("RSPU-5", defMessageType { msgPattern = "AFFIRM", msgArgs = [] } )
  , ("RSPU-6", defMessageType { msgPattern = "NEGATIVE", msgArgs = [], msgSuggestedSupp = ["SUPU-2"] } )
  , ("RSPU-7", defMessageType { msgPattern = "REQUEST FORWARDED", msgArgs = [] } )
  , ("RSPU-8", defMessageType { msgPattern = "CONFIRM REQUEST", msgArgs = [] } )
  , ("TXTU-1", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyR } )
  , ("TXTU-2", defMessageType { msgPattern = "$1", msgArgs = [ argText ] } )
  , ("TXTU-3", defMessageType { msgPattern = "$1", msgArgs = [ argText ] } )
  , ("TXTU-4", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyWU } )
  , ("TXTU-5", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyAN } )
  , ("HPPU-1", defMessageType { msgPattern = "LOGON ACCEPTED" } )
  , ("HPPU-2", defMessageType { msgPattern = "HANDOVER $1", msgArgs = [ argDataAuthority ] } )
  , ("HPPU-3", defMessageType { msgPattern = "LOGOFF" } )
  ]

downlinkMessages :: Map MessageTypeID MessageType
downlinkMessages = Map.fromList
    [ ( "SYSD-1", defMessageType { msgPattern = "ERROR $1", msgArgs = [ argText ] } )
    , ( "SYSD-2", defMessageType { msgPattern = "LOGICAL ACKNOWLEDGEMENT", msgArgs = [] } )
    , ( "SYSD-3", defMessageType { msgPattern = "NOT CURRENT DATA AUTHORITY", msgArgs = [] } )
    , ( "SYSD-4", defMessageType { msgPattern = "CURRENT DATA AUTHORITY", msgArgs = [] } )
    , ( "SYSD-5", defMessageType { msgPattern = "NOT AUTHORIZED NEXT DATA AUTHORITY $1 $2", msgArgs = [ argDataAuthority , argDataAuthority ] } )
    , ( "SYSD-6", defMessageType { msgPattern = "MESSAGE RECEIVED TOO LATE, RESEND MESSAGE OR CONTACT BY VOICE", msgArgs = [] } )
    , ( "SYSD-7", defMessageType { msgPattern = "AIRCRAFT CPDLC INHIBITED", msgArgs = [] } )
    , ( "RTED-1", defMessageType { msgPattern = "REQUEST DIRECT TO $1", msgArgs = [ argNavpos ], msgReplyOpts = ReplyY } )
    , ( "RTED-2", defMessageType { msgPattern = "REQUEST $1", msgArgs = [ argRoute ], msgReplyOpts = ReplyY } )
    , ( "RTED-3", defMessageType { msgPattern = "REQUEST CLEARANCE $1", msgArgs = [ argRoute ], msgReplyOpts = ReplyY } )
    , ( "RTED-4", defMessageType { msgPattern = "REQUEST $1 CLEARANCE", msgArgs = [ argClearanceType ], msgReplyOpts = ReplyY } )
    , ( "RTED-5", defMessageType { msgPattern = "POSITION REPORT $1", msgArgs = [ argPosrep ], msgReplyOpts = ReplyY } )
    , ( "RTED-6", defMessageType { msgPattern = "REQUEST HEADING $1", msgArgs = [ argDegrees ], msgReplyOpts = ReplyY } )
    , ( "RTED-7", defMessageType { msgPattern = "REQUEST GROUND TRACK $1", msgArgs = [ argDegrees ], msgReplyOpts = ReplyY } )
    , ( "RTED-8", defMessageType { msgPattern = "WHEN CAN WE EXPECT BACK ON ROUTE", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "RTED-9", defMessageType { msgPattern = "ASSIGNED ROUTE $1", msgArgs = [ argRoute ] } )
    , ( "RTED-10", defMessageType { msgPattern = "ETA $1 TIME $2", msgArgs = [ argNavpos , argTime ] } )
    , ( "LATD-1", defMessageType { msgPattern = "REQUEST OFFSET $1 $2 OF ROUTE", msgArgs = [ argDistance , argDirection ], msgReplyOpts = ReplyY } )
    , ( "LATD-2", defMessageType { msgPattern = "REQUEST WEATHER DEVIATION UP TO $1 OF ROUTE", msgArgs = [ argDistance ], msgReplyOpts = ReplyY } )
    , ( "LATD-3", defMessageType { msgPattern = "CLEAR OF WEATHER", msgArgs = [] } )
    , ( "LATD-4", defMessageType { msgPattern = "BACK ON ROUTE", msgArgs = [] } )
    , ( "LATD-5", defMessageType { msgPattern = "DIVERTING TO $1 VIA $2", msgArgs = [ argNavpos , argRoute ], msgReplyOpts = ReplyY } )
    , ( "LATD-6", defMessageType { msgPattern = "OFFSETTING $1 $2 OF ROUTE", msgArgs = [ argDistance , argDirection ], msgReplyOpts = ReplyY } )
    , ( "LATD-7", defMessageType { msgPattern = "DEVIATING $1 $2 OF ROUTE", msgArgs = [ argDistance , argDirection ], msgReplyOpts = ReplyY } )
    , ( "LATD-8", defMessageType { msgPattern = "PASSING $1", msgArgs = [ argNavpos ] } )
    , ( "LVLD-1", defMessageType { msgPattern = "REQUEST LEVEL $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyY } )
    , ( "LVLD-2", defMessageType { msgPattern = "REQUEST CLIMB TO $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyY } )
    , ( "LVLD-3", defMessageType { msgPattern = "REQUEST DESCENT TO $1", msgArgs = [ argFlAlt ], msgReplyOpts = ReplyY } )
    , ( "LVLD-4", defMessageType { msgPattern = "AT $1 REQUEST $2", msgArgs = [ argNavpos , argFlAlt ], msgReplyOpts = ReplyY } )
    , ( "LVLD-5", defMessageType { msgPattern = "AT TIME $1 REQUEST $2", msgArgs = [ argTime , argFlAlt ], msgReplyOpts = ReplyY } )
    , ( "LVLD-6", defMessageType { msgPattern = "WHEN CAN WE EXPECT LOWER LEVEL", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "LVLD-7", defMessageType { msgPattern = "WHEN CAN WE EXPECT HIGHER LEVEL", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "LVLD-8", defMessageType { msgPattern = "LEAVING LEVEL $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-9", defMessageType { msgPattern = "MAINTAINING LEVEL $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-10", defMessageType { msgPattern = "REACHING BLOCK $1 TO $2", msgArgs = [ argFlAlt , argFlAlt ] } )
    , ( "LVLD-11", defMessageType { msgPattern = "ASSIGNED LEVEL $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-12", defMessageType { msgPattern = "PREFERRED LEVEL $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-13", defMessageType { msgPattern = "CLIMBING TO $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-14", defMessageType { msgPattern = "DESCENDING TO $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-15", defMessageType { msgPattern = "WE CAN ACCEPT $1 AT TIME $2", msgArgs = [ argFlAlt , argTime ] } )
    , ( "LVLD-16", defMessageType { msgPattern = "WE CAN ACCEPT $1 AT $2", msgArgs = [ argFlAlt , argNavpos ] } )
    , ( "LVLD-17", defMessageType { msgPattern = "WE CANNOT ACCEPT $1", msgArgs = [ argFlAlt ] } )
    , ( "LVLD-18", defMessageType { msgPattern = "TOP OF DESCENT $1 TIME $2", msgArgs = [ argText , argTime ] } )
    , ( "ADVD-1", defMessageType { msgPattern = "SQUAWKING $1", msgArgs = [ argXpdr ] } )
    , ( "ADVD-2", defMessageType { msgPattern = "TRAFFIC $1", msgArgs = [ argText ] } )
    , ( "SPDD-1", defMessageType { msgPattern = "REQUEST $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyY } )
    , ( "SPDD-2", defMessageType { msgPattern = "WHEN CAN WE EXPECT $1", msgArgs = [ argSpeed ], msgReplyOpts = ReplyY } )
    , ( "SPDD-3", defMessageType { msgPattern = "$1 SPEED $2", msgArgs = [ argSpeedType , argSpeed ] } )
    , ( "SPDD-4", defMessageType { msgPattern = "ASSIGNED SPEED $1", msgArgs = [ argSpeed ] } )
    , ( "SPDD-5", defMessageType { msgPattern = "WE CAN ACCEPT $1 AT TIME $2", msgArgs = [ argSpeed , argTime ] } )
    , ( "SPDD-6", defMessageType { msgPattern = "WE CANNOT ACCEPT $1", msgArgs = [ argSpeed ] } )
    , ( "RSPD-1", defMessageType { msgPattern = "WILCO", msgArgs = [] } )
    , ( "RSPD-2", defMessageType { msgPattern = "UNABLE", msgArgs = [], msgSuggestedSupp = ["SUPD-1"] } )
    , ( "RSPD-3", defMessageType { msgPattern = "STANDBY", msgArgs = [] } )
    , ( "RSPD-4", defMessageType { msgPattern = "ROGER", msgArgs = [] } )
    , ( "RSPD-5", defMessageType { msgPattern = "AFFIRM", msgArgs = [] } )
    , ( "RSPD-6", defMessageType { msgPattern = "NEGATIVE", msgArgs = [], msgSuggestedSupp = ["SUPD-1"] } )
    , ( "COMD-1", defMessageType { msgPattern = "REQUEST VOICE CONTACT $1", msgArgs = [ argFreq ], msgReplyOpts = ReplyY } )
    , ( "COMD-2", defMessageType { msgPattern = "RELAY FROM $1", msgArgs = [ argText ], msgReplyOpts = ReplyN } )
    , ( "EMGD-1", defMessageType { msgPattern = "PAN PAN PAN", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "EMGD-2", defMessageType { msgPattern = "MAYDAY MAYDAY MAYDAY", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "EMGD-3", defMessageType { msgPattern = "$1 ENDURANCE AND $2 POB", msgArgs = [ argEndurance , argInteger ], msgReplyOpts = ReplyY } )
    , ( "EMGD-4", defMessageType { msgPattern = "CANCEL EMERGENCY", msgArgs = [], msgReplyOpts = ReplyY } )
    , ( "SUPD-1", defMessageType { msgPattern = "DUE {!TO} $1", msgArgs = [ argReason ] } )
    , ( "TXTD-1", defMessageType { msgPattern = "$1", msgArgs = [ argText ], msgReplyOpts = ReplyY } )
    , ( "TXTD-2", defMessageType { msgPattern = "$1", msgArgs = [ argText ] } )
    , ( "HPPD-1", defMessageType { msgPattern = "REQUEST LOGON", msgReplyOpts = ReplyY } )
    , ( "HPPD-2", defMessageType { msgPattern = "LOGOFF" } )
    ]

smokeTestMessages :: [ByteString]
smokeTestMessages = [
    "REQUEST LOGON"
    -- "AIR TRAFFIC SERVICE TERMINATED MONITOR UNICOM 122.800",
    -- "AT @1257@ DESCEND TO AND MAINTAIN @FL290",
    -- "ATC REQUEST STATUS . . FSM 1104 230123 EFHK @FIN7RA@ CDA RECEIVED @CLEARANCE CONFIRMED",
    -- "CALL ATC ON FREQUENCY",
    -- "CALL ME ON VOICE",
    -- "CLEARANCE DELIVERED VIA TELEX",
    -- "CLEARED TO DESTINATION  DEPART VIA ARA1X RWY 16 CLIMB FL150 SQK 1447",
    -- "CLEARED TO DESTINATION  DEPART VIA PIMOS2H RWY 13 CLIMB FL90 SQK 2621 ATIS INFO D",
    -- "CLEARED TO @GCLP@ VIA @OSPEN4C OSPEN FP ROUTE CLIMB INIT 5000FT SQUAWK 4024",
    -- "CLEARED TO @IFR FLIGHT TO PARIS@ VIA @RTE 1 AS FILED",
    -- "CLEARED TO @KORD@ VIA @HYLND CAM PAYGE Q822 FNT WYNDE2 EMMMA@ SQUAWK @2654@",
    -- "CLIMB TO AND MAINTAIN @30000FT@ REPORT LEVEL @30000FT@",
    -- "CLIMB TO AND MAINTAIN @32000FT@ REPORT LEVEL @32000FT@",
    -- "CLIMB TO AND MAINTAIN @34000FT@ REPORT LEVEL @34000FT@",
    -- "CLIMB TO AND MAINTAIN @36000FT@ REPORT LEVEL @36000FT@",
    -- "CLIMB TO AND MAINTAIN @FL400@ REPORT LEVEL @FL400@",
    -- "CLIMB TO @FL240",
    -- "CLIMB TO @FL290@",
    -- "CLIMB TO @FL300@",
    -- "CLIMB TO @FL300@ | PROCEED DIRECT TO @VELIS@",
    -- "CLIMB TO @FL310@ CLIMB AT @1000 FT|MIN MAXIMUM",
    -- "CLIMB TO @FL320",
    -- "CLIMB TO @FL330",
    -- "CLIMB TO @FL330@ RESUME OWN NAVIGATION",
    -- "CLIMB TO @FL340",
    -- "CLIMB TO @FL350",
    -- "CLIMB TO @FL350@",
    -- "CLIMB TO @FL360",
    -- "CLIMB TO @FL360@",
    -- "CLIMB TO @FL370",
    -- "CLIMB TO @FL380",
    -- "CLIMB TO @FL390",
    -- "CLRD TO @ZBAA@ OFF @01@ VIA @YIN1A@ SQUAWK @@ INITIAL ALT @@ NEXT FREQ @121.950@",
    -- "CONTACT @121.375@_@COPENHAGEN CTL",
    -- "CONTACT @121.375@_@COPENHAGEN CTR",
    -- "CONTACT @127.725@_@EDGG_H1F_CTR",
    -- "CONTACT @127.725@_@EDGG_HEF_CTR",
    -- "CONTACT @128.100@_@PARIS CTL",
    -- "CONTACT @128.625@_@SWEDEN CTL",
    -- "CONTACT @129.425@_@LON_S_CTR",
    -- "CONTACT @129.675@_@LGGG_CTR",
    -- "CONTACT @130.000@_@ADRIA",
    -- "CONTACT @131.225@_@SOFIA",
    -- "CONTACT @131.900@_@NAT_FSS",
    -- "CONTACT @132.850@_@LPPC_E_CTR",
    -- "CONTACT @132.975@_@LECM_CTR",
    -- "CONTACT @134.125@_@LTC_S_CTR",
    -- "CONTACT @134.700@_@EDYY_J_CTR",
    -- "CONTACT @134.700@_@MAASTRICHT CTR",
    -- "CONTACT @136.955@ @@",
    -- "CONTACT @EDGD 125.200@_@LANGEN CTR",
    -- "CONTACT @EDGG 124.725@_@LANGEN CTR",
    -- "CONTACT EDGG_CTR @136.955@",
    -- "CONTACT @EDYC 133.950@_@MAASTRICHT CTR",
    -- "CONTACT @EDYJ 134.700@_@MAASTRICHT CTR",
    -- "CONTACT @EGPX 135.525@_@SCOTTISH CTL",
    -- "CONTACT @EKDB 121.375@_@COPENHAGEN CTR",
    -- "CONTACT @EUROCONTROL@ @135.125@",
    -- "CONTACT @EUWN 135.125@_@EUROCONTROL CTL",
    -- "CONTACT @GENEVA ARRIVAL@ @131.325@",
    -- "CONTACT LECM_R1_CTR @135.700@",
    -- "CONTACT @LFFF@ @128.100@",
    -- "CONTACT LFMM_NW_CTR @123.805@",
    -- "CONTACT @LFXX 128.100@_@PARIS CTL",
    -- "CONTACT @LON@ @129.425@",
    -- "CONTACT @LONC 127.100@_@LONDON CTL",
    -- "CONTACT @LONDON CONTROL@ @127.100",
    -- "CONTACT @LONN 133.700@_@LONDON CTL",
    -- "CONTACT @LONS 129.425@_@LONDON CTL",
    -- "CONTACT @LPZE 132.850@_@LISBOA CTL",
    -- "CONTACT ME BY RADIO I HAVE BEEN TRYING TO CALL YOU",
    -- "CONTACT @REIMS CONTROL@ @128.300@",
    -- "CONTLR CHANGE RESEND REQ OR REVERT TO VOICE",
    -- "CURRENT ATC UNIT@_@ADRA@_@ADRIA",
    -- "CURRENT ATC UNIT@_@ADRW@_@ADRIA",
    -- "CURRENT ATC UNIT@_@BIRD@_@REYKJAVIK OCA",
    -- "CURRENT ATC UNIT@_@CBRA@_@BARCELONA CTL",
    -- "CURRENT ATC UNIT@_@CMRM@_@MADRID CTL",
    -- "CURRENT ATC UNIT@_@EDGD@_@LANGEN CTR",
    -- "CURRENT ATC UNIT@_@EDGG@_@LANGEN CTR",
    -- "CURRENT ATC UNIT@_@EDUW@_@RHEIN RADAR CTR",
    -- "CURRENT ATC UNIT@_@EDYC@_@MAASTRICHT CTR",
    -- "CURRENT ATC UNIT@_@EDYJ@_@MAASTRICHT CTR",
    -- "CURRENT ATC UNIT@_@EFIN@_@HELSINKI CTL",
    -- "CURRENT ATC UNIT@_@EGPX",
    -- "CURRENT ATC UNIT@_@EGPX@_@SCOTTISH CONTROL",
    -- "CURRENT ATC UNIT@_@EGPX@_@SCOTTISH CTL",
    -- "CURRENT ATC UNIT@_@EISE@_@SHANNON CTL",
    -- "CURRENT ATC UNIT@_@EKCH",
    -- "CURRENT ATC UNIT@_@EKCH1",
    -- "CURRENT ATC UNIT@_@EKCH2",
    -- "CURRENT ATC UNIT@_@EKDB@_@COPENHAGEN CTL",
    -- "CURRENT ATC UNIT@_@EPWW@_@WARSZAWA RADAR",
    -- "CURRENT ATC UNIT@_@ESOS@_@SWEDEN CTL",
    -- "CURRENT ATC UNIT@_@EUWN",
    -- "CURRENT ATC UNIT@_@LGGG@_@ATHINAI",
    -- "CURRENT ATC UNIT@_@LONC@_@LONDON CTL",
    -- "CURRENT ATC UNIT@_@LONE@_@LONDON CTL",
    -- "CURRENT ATC UNIT@_@LONM@_@LONDON CTL",
    -- "CURRENT ATC UNIT@_@LONN@_@LONDON CTL",
    -- "CURRENT ATC UNIT@_@LONS@_@LONDON CTL",
    -- "CURRENT ATC UNIT@_@LOVE@_@WIEN",
    -- "CURRENT ATC UNIT@_@LPZE@_@LISBOA CTL",
    -- "CURRENT ATC UNIT@_@LPZW@_@LISBOA CTL",
    -- "CURRENT ATC UNIT@_@LTBB@_@ANKARA CTR",
    -- "DESCEND TO @12000 FT",
    -- "DESCEND TO @3000 FT",
    -- "DESCEND TO @4000 FT",
    -- "DESCEND TO AND MAINTAIN @FL200@",
    -- "DESCEND TO @FL080",
    -- "DESCEND TO @FL100",
    -- "DESCEND TO @FL110",
    -- "DESCEND TO @FL110@",
    -- "DESCEND TO @FL120",
    -- "DESCEND TO @FL140",
    -- "DESCEND TO @FL180",
    -- "DESCEND TO @FL200",
    -- "DESCEND TO @FL210",
    -- "DESCEND TO @FL250",
    -- "DESCEND TO @FL260",
    -- "DESCEND TO @FL280",
    -- "DESCEND TO @FL290",
    -- "DESCEND TO @FL300",
    -- "DESCEND TO @FL320",
    -- "DESCEND TO @FL330",
    -- "DESCEND TO @FL340",
    -- "DESCEND TO @FL350",
    -- "DESCEND TO REACH @FL190@ BY @DJL@",
    -- "DESCENT FL100",
    -- "DOWNLINK REJECTED - @USE VOICE",
    -- "ERROR @REVERT TO VOICE PROCEDURES",
    -- "FLIGHT PLAN NOT HELD",
    -- "FLY HEADING @120",
    -- "FREE SPEED",
    -- "FSM 1133 230123 EDVK @AIB1010@ RCD REJECTED @TYPE MISMATCH @UPDATE RCD AND RESEND",
    -- "FSM 1140 230123 EDVK @AIB1010@ RCD REJECTED @REVERT TO VOICE PROCEDURES",
    -- "FSM 1337 230122 EDDK @WAT585@ RCD RECEIVED @REQUEST BEING PROCESSED @STANDBY",
    -- "FSM 1648 230122 EDDM @DLH09W@ RCD RECEIVED @REQUEST BEING PROCESSED @STANDBY",
    -- "HANDOVER @EDGD",
    -- "HANDOVER @EGPX",
    -- "INCREASE SPEED TO @250 KTS",
    -- "INCREASE SPEED TO @M.74",
    -- "LEAVING AIRSPACE MONITOR UNICOM 122.8",
    -- "LOGOFF",
    -- "LOGON ACCEPTED",
    -- "MAINTAIN @210 KTS",
    -- "MAINTAIN @FL100",
    -- "MAINTAIN @FL280",
    -- "MAINTAIN @M.72",
    -- "MAINTAIN @M.75",
    -- "MAINTAIN @M77@",
    -- "MESSAGE NOT SUPPORTED BY THIS ATS UNIT",
    -- "MONITOR @UNICOM@ @122.8@",
    -- "MONITOR UNICOM 122.8",
    -- "MONITOR UNICOM @122.800@",
    -- "MONITOR UNICOM 122.800",
    -- "MONITOR UNICOM 122.8 BYE",
    -- "MONITOR UNICOM 122.8. NICE DAY",
    -- "NEXT DATA AUTHORITY @EKCH2@",
    -- "OCEAN REQUEST ENTRY POINT: BALIX AT:1431 REQ: M.78 FL360  BALIX 61N014W EXIT",
    -- "PLS CONTACT ME BY QQ",
    -- "POSITION AM059 AT 1638 FL 85M EST RINIS AT 1656 NEXT IDESI",
    -- "PROCEED DIRECT TO @AHVEC@",
    -- "PROCEED DIRECT TO @HELEN@ DESCEND TO @FL200",
    -- "REDUCE SPEED TO @M.77",
    -- "REQUEST 10000",
    -- "REQUEST AGAIN WITH NEXT UNIT",
    -- "REQUEST CLB TO 34000FT",
    -- "REQUEST CRUISE CLIMB TO FL380",
    -- "REQUEST DEPARTURE CLEARANCE",
    -- "REQUEST DIRECT TO@EVRIN",
    -- "REQUEST DIRECT TO HMM",
    -- "REQUEST DIR TO TOPTU",
    -- "REQUEST FL110",
    -- "REQUEST FL320 DUE TO WEATHER",
    -- "REQUEST FL360 DUE TO AIRCRAFT PERFORMANCE",
    -- "REQUEST KBOS-KORD KBOS.HYLND.CAM.PAYGE.Q822.FNT.WYNDE2.EMMMA.KORD",
    -- "REQUEST KMCI-KATL KMCI.KATL",
    -- "REQUEST LOGON",
    -- "REQUEST URB7A",
    -- "REQUEST VOICE CONTACT ON 126.425",
    -- "RESUME NORMAL SPEED",
    -- "REVERT TO VOICE",
    -- "ROGER",
    -- "SERVICE TERMINATED",
    -- "SERVICE TERMINATED FREQ CHG APPROVED",
    -- "SERVICE TERMINATED. MONITOR UNICOM 122.800",
    -- "SQUAWK @1000",
    -- "SQUAWK IDENT",
    -- "STANDBY",
    -- "STBY",
    -- "STDBY",
    -- "THANKS FOR USING MAASTRICHT CPDLC",
    -- "THANK YOU FOR USING CPDLC. BEST REGARDS FROM PLVACC.",
    -- "TIMEDOUT RESEND REQUEST OR REVERT TO VOICE",
    -- "UNABLE",
    -- "UNABLE AT EGAA",
    -- "UNABLE DUE AIRSPACE",
    -- "UNABLE DUE TO AIRSPACE",
    -- "UNABLE DUE TRAFFIC",
    -- "UNABLE REVERT TO VOICE",
    -- "WHEN CAN WE EXPECT CLIMB TO CRZ ALT 32000",
    -- "WHEN CAN WE EXPECT HIGHER ALT",
    -- "WHEN CAN WE EXPECT LOWER ALT",
    -- "WHEN CAN WE EXPECT LOWER ALT AT PILOT DISCRETION",
    -- "WHEN READY DESCEND TO REACH FL250 AT RIMET",
    -- "WILCO",
    -- "YOU ARE LEAVING MY AIRSPACE NO FURTHER ATC MONITOR UNICOM 122.800 BYE BYE"
  ]

runSmokeTests :: IO ()
runSmokeTests = do
  let messages = smokeTestMessages
  -- let messages = ["PROCEED DIRECT TO @HELEN@ DESCEND TO @FL200"]
  forM_ messages $ \msg -> do
    let matching = parseMessage Nothing allMessageTypes msg
    -- let matching = concat $ flip map (Map.toList allMessageTypes) $ \(msgID, msgTy) -> do
    --       case matchMessage msgTy msg (msgReplyOpts msgTy) of
    --         Nothing ->
    --           []
    --         Just matches ->
    --           [(msgID, matches)]
    putStrLn $ BS8.unpack msg ++ ":"
    case matching of
      [] ->
        putStrLn "  --- NO MATCHES ---"
      (x:_xs) -> do
        forM_ x $ \(msgID, args) -> do
          BS8.putStrLn $ msgID <> ": " <> renderMessage allMessageTypes msgID args

bold :: ByteString
bold = "\27[1m"

regular :: ByteString
regular = "\27[22m"
