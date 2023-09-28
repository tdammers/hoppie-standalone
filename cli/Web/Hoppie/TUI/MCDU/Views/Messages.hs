{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.Messages
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time
import Data.Word
import Text.Printf

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
{-# ANN module ("HLint: ignore Move brackets to avoid $" :: String) #-}

dlkMenuView :: MCDUView
dlkMenuView = defView
  { mcduViewTitle = "DLK MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("RECVD MSGS", loadView dlkUplinkLogView))
      , (LSKL 1, ("SENT MSGS", loadView dlkDownlinkLogView))
      , (LSKL 3, ("TELEX", clearTelexBody >> loadView (telexSendView False)))
      , (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
      , (LSKR 0, ("ATIS", loadView $ infoMenuView "ATIS" "VATATIS"))
      , (LSKR 1, ("METAR", loadView $ infoMenuView "METAR" "METAR"))
      , (LSKR 2, ("TAF", loadView $ infoMenuView "TAF" "TAF"))
      , (LSKR 3, ("DCL", loadView (clearanceSendView False)))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK (LSKR 5)
  }

data CPDLCEditorState =
  CPDLCEditorState
    { cpdlcEditorVars :: Map (Int, Word) ByteString
    }

defCPDLCEditorState :: CPDLCEditorState
defCPDLCEditorState =
  CPDLCEditorState
    { cpdlcEditorVars = mempty
    }

loadCpdlcComposeViewByID :: CPDLC.MessageTypeID -> Maybe ByteString -> Maybe Word -> MCDU ()
loadCpdlcComposeViewByID tyID toMay mrnMay = do
  let tyMay = Map.lookup tyID CPDLC.downlinkMessages
  case tyMay of
    Just ty -> do
      loadView =<< mkCpdlcComposeView tyID ty toMay mrnMay
    Nothing -> do
      scratchWarn "INVALID"

mkCpdlcComposeView :: CPDLC.MessageTypeID -> CPDLC.MessageType -> Maybe ByteString -> Maybe Word -> MCDU MCDUView
mkCpdlcComposeView tyID ty toMay mrnMay = do
  editorStateVar <- liftIO $ newMVar defCPDLCEditorState
  let supTys = [ fromMaybe (error $ "Invalid supplemental CPDLC message type: " ++ show supTy)
                    $ Map.lookup supTy CPDLC.downlinkMessages
               | supTy <- supTyIDs
               ]
      supTyIDs = CPDLC.msgSuggestedSupp ty ++ ["TXTD-2"]
      tys = ty : supTys
  let getVar partIndex index = Map.lookup (partIndex, index) . cpdlcEditorVars <$> liftIO (readMVar editorStateVar)
      setVar partIndex index (Just val) = do
        liftIO $
          modifyMVar_ editorStateVar (\s -> return s
            { cpdlcEditorVars = Map.insert (partIndex, index) val (cpdlcEditorVars s) })
        return True
      setVar partIndex index Nothing = do
        liftIO $
          modifyMVar_ editorStateVar (\s -> return s
            { cpdlcEditorVars = Map.delete (partIndex, index) (cpdlcEditorVars s) })
        return True
      entrySpec = map getEntryItems tys
      hydrate = do
        vars <- cpdlcEditorVars <$> liftIO (readMVar editorStateVar)
        return $ concat
          [ [ ( label
              , partIndex
              , varMay
              , varMay >>= \(_, i) -> Map.lookup (partIndex, i) vars
              )
            | (label, varMay) <- part
            ]
          | (partIndex, part) <- zip [0..] entrySpec
          ]
  return defView
            { mcduViewTitle = "CPDLC COMPOSE"
            , mcduViewNumPages = 1
            , mcduViewOnLoad = do
                entryItems <- hydrate
                curPage <- gets (mcduViewPage . mcduView)
                let (numPages, curEntryItems) =
                      paginate numLSKs curPage entryItems
                    lastPage = curPage == numPages - 1
                modifyView $ \v -> v
                    { mcduViewLSKBindings = mempty
                    , mcduViewNumPages = numPages
                    , mcduViewDraw = do
                        zipWithM_ (\y (label', _, varMay, valMay) -> do
                            let label = case (label', varMay) of
                                  ("", Just (a, _)) ->
                                    colorize cyan $
                                      "(" <> (BS8.pack . map toUpper . drop 3 . show $ CPDLC.argTy a) <> ")"
                                  _ ->
                                    colorize white label'
                            mcduPrintColored 1 (y * 2 + 1) label
                            forM_ varMay $ \(a, _) -> do
                              let defValFmt = case CPDLC.argTy a of
                                    CPDLC.ArgText -> BS.replicate (screenW-2) (ord8 '-')
                                    CPDLC.ArgReason -> BS.replicate (screenW-2) (ord8 '-')
                                    _ -> "----"
                              let valFmt = fromMaybe defValFmt valMay
                              mcduPrintR (screenW - 1) (y * 2 + 2) green valFmt
                          )
                          [0 .. numLSKs] curEntryItems
                    }
                let mkBinding n (_, partIndex, varMay, _) = do
                      forM_ varMay $ \(_, index) -> do
                        addLskBinding (LSKR n) "" $ do
                          scratchInteract
                            (setVar partIndex index)
                            (getVar partIndex index)
                          reloadView
                zipWithM_ mkBinding [0 .. numLSKs] curEntryItems
                when lastPage $ do
                  vars <- liftIO $ cpdlcEditorVars <$> readMVar editorStateVar
                  addLskBinding (LSKR 4) "SEND"
                    (sendCpdlc (tyID : supTyIDs) toMay mrnMay vars >>= \case
                      True -> loadView cpdlcMessageLogView
                      False -> reloadView
                    )
                  loadUplinkLSK (LSKR 5)
            }

getEntryItems :: CPDLC.MessageType -> [(ByteString, Maybe (CPDLC.ArgSpec, Word))]
getEntryItems ty =
  go (CPDLC.messagePatternItems $ CPDLC.msgPattern ty)
  where
    go :: [CPDLC.MessagePatternItem Word] -> [(ByteString, Maybe (CPDLC.ArgSpec, Word))]
    go [] = []
    go xs =
      let literalLines :: [ByteString]
          literalLines =
            lineWrap (screenW - 2) .
            wordJoin .
            concatMap getLiteralBS .
            takeWhile isLiteral $
            xs
      in
        case (literalLines, dropWhile isLiteral xs) of
          (_, []) ->
            [(lit, Nothing) | lit <- literalLines]
          ([], CPDLC.MessageArg i:xs') ->
            let argSpec = fromMaybe CPDLC.argText $ listToMaybe $ drop (fromIntegral i - 1) $ CPDLC.msgArgs ty
            in
              ("", Just (argSpec, i)) : go xs'
          (_, CPDLC.MessageArg i:xs') ->
            let argSpec = fromMaybe CPDLC.argText $ listToMaybe $ drop (fromIntegral i - 1) $ CPDLC.msgArgs ty
            in
              [(lit, Nothing) | lit <- init literalLines] ++
              [(lit, Just (argSpec, i)) | lit <- drop (length literalLines - 1) literalLines] ++
              go xs'
          (_, _:xs') ->
            [(lit, Nothing) | lit <- literalLines] ++
            go xs'

    isLiteral :: CPDLC.MessagePatternItem Word -> Bool
    isLiteral (CPDLC.MessageLiteral {}) = True
    isLiteral (CPDLC.MessageOptional {}) = True
    isLiteral _ = False

    getLiteralBS :: CPDLC.MessagePatternItem Word -> [ByteString]
    getLiteralBS (CPDLC.MessageLiteral lit) = wordSplit lit
    getLiteralBS (CPDLC.MessageOptional False xs) = xs
    getLiteralBS _ = []

telexSendView :: Bool -> MCDUView
telexSendView sent = defView
  { mcduViewTitle = "TELEX"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 5, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      callsign <- lift getCallsign
      unless sent $ do
        addLskBinding (LSKR 4) "SEND" $ do
          sendSuccess <- sendTelex
          when sendSuccess $ do
            loadView (telexSendView True)

      addLskBinding (LSKR 0) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduTelexRecipient = val }))
          (gets mcduTelexRecipient)
        reloadView

      addLskBinding (LSKL 1) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduTelexBody = val }))
          (gets mcduTelexBody)
        reloadView

      recipientStr <- fromMaybe "------" <$> gets mcduTelexRecipient
      bodyStr <- fromMaybe (BS.replicate (screenW - 2) (ord8 '-')) <$> gets mcduTelexBody
      let bodyLines = lineWrap (screenW - 2) bodyStr
      modifyView $ \v -> v {
        mcduViewDraw = do
          mcduPrint 1 2 white callsign
          mcduPrintR (screenW - 1) 2 green recipientStr
          zipWithM_
            (\n l -> mcduPrint 1 (n + 4) green l)
            [0 .. numLSKs - 1] bodyLines
      }
      loadUplinkLSK (LSKR 5)
  }

clearanceSendView :: Bool -> MCDUView
clearanceSendView sent = defView
  { mcduViewTitle = "DATALINK CLEARANCE"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 5, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      callsign <- lift getCallsign

      ctype <- gets mcduClearanceType
      when (isNothing ctype) $ do
        atype <- gets mcduAircraftType
        modify $ \s -> s
          { mcduClearanceType = atype }
        
      unless sent $ do
        addLskBinding (LSKR 4) "SEND" $ do
          sendSuccess <- sendClearanceRequest
          when sendSuccess $ do
            loadView (clearanceSendView True)

      addLskBinding (LSKL 1) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceType = val }))
          (gets mcduClearanceType)
        reloadView

      addLskBinding (LSKL 2) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceStand = val }))
          (gets mcduClearanceStand)
        reloadView

      addLskBinding (LSKL 3) "" $ do
        scratchInteract 
          (\case
              Nothing -> True <$ (modify $ \s -> s { mcduClearanceAtis = Nothing })
              Just val -> case BS.unpack val of
                [letter] -> True <$ (modify $ \s -> s { mcduClearanceAtis = Just letter })
                _ -> return False
          )
          (fmap BS.singleton <$> gets mcduClearanceAtis)
        reloadView

      addLskBinding (LSKR 0) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceFacility = val }))
          (gets mcduClearanceFacility)
        reloadView

      addLskBinding (LSKR 1) "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceDestination = val }))
          (gets mcduClearanceDestination)
        reloadView

      typeStr <- fromMaybe "------" <$> gets mcduClearanceType
      facilityStr <- fromMaybe "----" <$> gets mcduClearanceFacility
      destinationStr <- fromMaybe "----" <$> gets mcduClearanceDestination
      atisStr <- maybe "-" BS.singleton <$> gets mcduClearanceAtis
      standStr <- fromMaybe "-" <$> gets mcduClearanceStand

      modifyView $ \v -> v {
        mcduViewDraw = do
          mcduPrint 1 1 white "CALLSIGN"
          mcduPrint 1 2 green callsign
          mcduPrint 1 3 white "TYPE"
          mcduPrint 1 4 green typeStr
          mcduPrint 1 5 white "GATE"
          mcduPrint 1 6 green standStr
          mcduPrint 1 7 white "ATIS"
          mcduPrint 1 8 green atisStr
          mcduPrintR (screenW - 1) 1 white "FACILITY"
          mcduPrintR (screenW - 1) 2 green facilityStr
          mcduPrintR (screenW - 1) 3 white "DESTINATION"
          mcduPrintR (screenW - 1) 4 green destinationStr
      }
      loadUplinkLSK (LSKR 5)
  }

infoMenuView :: ByteString -> ByteString -> MCDUView
infoMenuView title infotype = defView
  { mcduViewTitle = title
  , mcduViewLSKBindings = Map.fromList
      [ (LSKR 4, ("SEND", sendInfoRequest infotype >> reloadView ))
      , (LSKL 5, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      refAirport <- gets (fromMaybe "----" . mcduReferenceAirport)
      addLskBinding (LSKR 0) "" (setReferenceAirport >> reloadView)
      modifyView $ \v -> v
        { mcduViewDraw =
            mcduPrintR (screenW - 1) (mcduLskY $ LSKR 0) green refAirport
        }
      loadUplinkLSK (LSKR 5)
  }

data MessageLogFilter
  = Uplinks
  | Downlinks
  | AllCPDLC
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

dlkUplinkLogView :: MCDUView
dlkUplinkLogView = messageLogView Uplinks

dlkDownlinkLogView :: MCDUView
dlkDownlinkLogView = messageLogView Downlinks

cpdlcMessageLogView :: MCDUView
cpdlcMessageLogView = messageLogView AllCPDLC

messageLogView :: MessageLogFilter -> MCDUView
messageLogView mlf = defView
  { mcduViewTitle = case mlf of
      Uplinks -> "RECVD DLK LOG"
      Downlinks -> "SENT DLK LOG"
      AllCPDLC -> "ATC MESSAGE LOG"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 5, case mlf of
              AllCPDLC -> ("ATC MENU", loadView atcMenuView)
              _ -> ("DLK MENU", loadView dlkMenuView)
        )
      ]
  , mcduViewOnLoad = do
      messages <- reverse . filter (if mlf == AllCPDLC then messageIsCPDLC else not . messageIsCPDLC)
                    <$> lift (case mlf of
                                AllCPDLC -> getAllMessages
                                Uplinks -> getUplinkMessages
                                Downlinks -> getDownlinkMessages
                             )
      curPage <- gets (mcduViewPage . mcduView)
      let (numPages, curMessages) = paginate (numLSKs - 1) curPage messages
      modifyView $ \s -> s
        { mcduViewNumPages = numPages
        , mcduViewLSKBindings =
            Map.fromList
              [ (LSKL 5, case mlf of
                      AllCPDLC -> ("ATC MENU", loadView atcMenuView)
                      _ -> ("DLK MENU", loadView dlkMenuView)
                )
              , (LSKR 5, ( "CLEAR LOG"
                    , do
                        case mlf of
                          AllCPDLC ->
                            lift clearCpdlcUplinksLog >>
                            lift clearCpdlcDownlinksLog
                          Uplinks ->
                            lift clearUplinksLog
                          Downlinks ->
                            lift clearDownlinksLog
                        reloadView
                    )
                )
              ]
            <>
            Map.fromList
              ( zip
                [LSKL 0 .. LSKL (numLSKs - 2)]
                [ ("", loadView $
                    messageView
                    (messageUID message) ) | message <- curMessages
                ]
              )
        , mcduViewDraw = do
            zipWithM_ (\n message -> do
                let (statusLine, msgText) = formatMessage message

                mcduPrintColored 1 (n * 2 + 1) statusLine
                mcduPrintColored 1 (n * 2 + 2) (wordJoin . wordSplit $ msgText)
              ) [0,1..] curMessages
        }
  }

formatCPDLC :: CPDLCMessage -> Colored ByteString
formatCPDLC =
  lineJoin . map formatCPDLCPart . cpdlcParts

formatCPDLCPart :: CPDLCPart -> Colored ByteString
formatCPDLCPart part =
  maybe
    (Colored [ColoredFragment red "INVALID CPDLC"])
    goTy
    tyMay
  where
    tyMay = Map.lookup (cpdlcType part) CPDLC.allMessageTypes

    argsColored =
      map (colorize green) (CPDLC.cpdlcArgs part)
      ++
      (repeat $ Colored [ColoredFragment red "N/A"])


    goTy :: CPDLC.MessageType -> Colored ByteString
    goTy _ | ("TXT" `BS.isPrefixOf` CPDLC.cpdlcType part) =
      colorize white $ wordJoin (CPDLC.cpdlcArgs part)
    goTy ty =
      wordJoin $ (flip map) (CPDLC.messagePatternItems $ CPDLC.msgPattern ty) $ \case
        CPDLC.MessageLiteral bs ->
          colorize white bs
        CPDLC.MessageArg i ->
          argsColored !! fromIntegral (i - 1)
        CPDLC.MessageOptional False bs ->
          colorize white $ wordJoin bs
        CPDLC.MessageOptional True _ ->
          mempty
        
      

messageColorCallsignStatus :: HoppieMessage -> (Word8, ByteString, String)
messageColorCallsignStatus = \case
  UplinkMessage m ->
    let cs = typedMessageCallsign $ payload m
    in case metaStatus m of
      NewUplink -> (green, cs, "NEW")
      OldUplink -> (white, cs, "OLD")
      RepliedUplink -> (white, cs, "REPL")
      OpenUplink -> (green, cs, "OPEN")
  DownlinkMessage m ->
    let cs = typedMessageCallsign $ payload m
    in case metaStatus m of
      UnsentDownlink -> (yellow, cs, "QUED")
      SentDownlink -> (cyan, cs, "SENT")
      ErrorDownlink -> (red, cs, "ERR")
      RepliedDownlink -> (blue, cs, "REPL")

messageColorTyStrBody :: Word8 -> HoppieMessage -> (Word8, String, Colored ByteString)
messageColorTyStrBody color' message =
  case typedMessagePayload $ messagePayload message of
    InfoPayload msg ->
      ( color'
      , "INFO"
      , colorize white msg
      )
    TelexPayload msg ->
      ( color'
      , "TELEX"
      , colorize white msg
      )
    CPDLCPayload cpdlc ->
      ( color'
      , "CPDLC",
      formatCPDLC cpdlc
      )
    UnsupportedPayload ty msg ->
      ( yellow
      , "UNSUPPORTED"
      , colorize yellow $ (BS8.pack . map toUpper . show) ty <> " " <> msg
      )
    ErrorPayload _ response err ->
      ( red
      , "ERROR"
      , colorize red $ BS8.pack err <> "\n" <> response
      )

formatMessage :: HoppieMessage -> (Colored ByteString, Colored ByteString)
formatMessage message =
  (colorize color statusRaw, body)
  where
    (color', callsign, statusStr) = messageColorCallsignStatus message
    (color, msgTyStr, body) = messageColorTyStrBody color' message
    daySecond = floor . utctDayTime $ messageTimestamp message
    (hours, minutes) = (daySecond `quot` 60 :: Int) `quotRem` 60
    statusRaw = BS8.pack $
                  printf "%02i%02iZ %-6s %-5s %-4s"
                    hours minutes
                    (map toUpper $ BS8.unpack callsign)
                    msgTyStr
                    statusStr

messageBindings :: HoppieMessage -> MCDU [(ByteString, MCDU ())]
messageBindings (UplinkMessage mtm) =
  case typedMessagePayload . payload $ mtm of
    CPDLCPayload cpdlc -> do
      let mrn = CPDLC.cpdlcMIN cpdlc
          sender = typedMessageCallsign . payload $ mtm
      case CPDLC.cpdlcParts cpdlc of
        [] -> return []
        (part:_) -> do
          let tyMay = Map.lookup (CPDLC.cpdlcType part) CPDLC.allMessageTypes
          let bindings3 =
                case tyMay of
                  Nothing ->
                    []
                  Just ty ->
                    cpdlcReplyOptBindings sender mrn (CPDLC.msgReplyOpts ty)
          parentMay <- lift $ getCpdlcParentDownlink mtm
          childMay <- lift $ getCpdlcChildDownlink mtm
          let bindings1 =
                case childMay of
                  Just child -> 
                    [("REPLY", loadView $ messageView (metaUID child))]
                  Nothing ->
                    bindings3
              bindings2 =
                case parentMay of
                  Just parent ->
                    [("REQUEST", loadView $ messageView (metaUID parent))]
                  Nothing ->
                    []
          return $ bindings1 ++ bindings2
    _ -> return []
messageBindings (DownlinkMessage mtm) =
  case typedMessagePayload . payload $ mtm of
    CPDLCPayload cpdlc -> do
      case CPDLC.cpdlcParts cpdlc of
        [] -> return []
        _ -> do
          parentMay <- lift $ getCpdlcParentUplink mtm
          childMay <- lift $ getCpdlcChildUplink mtm
          let bindings1 =
                case childMay of
                  Just child -> 
                    [("REPLY", loadView $ messageView (metaUID child))]
                  Nothing ->
                    []
              bindings2 =
                case parentMay of
                  Just parent ->
                    [("REQUEST", loadView $ messageView (metaUID parent))]
                  Nothing ->
                    []
          return $ bindings1 ++ bindings2
    _ -> return []

cpdlcReplyOptBindings :: ByteString -> Word -> CPDLC.ReplyOpts -> [ (ByteString, MCDU ()) ]
cpdlcReplyOptBindings sender mrn = \case
  CPDLC.ReplyAN ->
      [ ("AFFIRM", loadCpdlcComposeViewByID "RSPD-5" (Just sender) (Just mrn))
      , ("NEGATIVE", loadCpdlcComposeViewByID "RSPD-6" (Just sender) (Just mrn))
      , ("STANDBY", loadCpdlcComposeViewByID "RSPD-3" (Just sender) (Just mrn))
      ]
  CPDLC.ReplyWU ->
      [ ("WILCO", loadCpdlcComposeViewByID "RSPD-1" (Just sender) (Just mrn))
      , ("UNABLE", loadCpdlcComposeViewByID "RSPD-2" (Just sender) (Just mrn))
      , ("STANDBY", loadCpdlcComposeViewByID "RSPD-3" (Just sender) (Just mrn))
      ]
  CPDLC.ReplyR ->
      [ ("ROGER", loadCpdlcComposeViewByID "RSPD-4" (Just sender) (Just mrn))
      , ("UNABLE", loadCpdlcComposeViewByID "RSPD-2" (Just sender) (Just mrn))
      , ("STANDBY", loadCpdlcComposeViewByID "RSPD-3" (Just sender) (Just mrn))
      ]
  CPDLC.ReplyY ->
      [ ("FREE TEXT", loadCpdlcComposeViewByID "TXTD-2" (Just sender) (Just mrn))
      , ("STANDBY", loadCpdlcComposeViewByID "RSPD-3" (Just sender) (Just mrn))
      ]
  CPDLC.ReplyN ->
      []

messageView :: Word -> MCDUView
messageView uid =
  defView
    { mcduViewTitle = "MESSAGE"
    , mcduViewOnLoad = do
        messageMay <- lift $ getMessage uid
        case messageMay of
          Nothing ->
            modifyView $ \v -> v
              { mcduViewDraw = do
                  mcduPrintC (screenW `div` 2) (screenH `div` 2) red
                    "MESSAGE NOT FOUND"
              , mcduViewNumPages = 1
              , mcduViewPage = 0
              }
          Just message -> do
            unreadDLK <- gets mcduUnreadDLK
            unreadCPDLC <- gets mcduUnreadCPDLC
            when (Just (messageUID message) == unreadDLK) $ do
              modify $ \s -> s { mcduUnreadDLK = Nothing }
            when (Just (messageUID message) == unreadCPDLC) $ do
              modify $ \s -> s { mcduUnreadCPDLC = Nothing }
            case message of
              UplinkMessage {} ->
                lift $ setUplinkStatus (messageUID message) OldUplink
              DownlinkMessage {} ->
                return ()

            let (statusLine, msgText) = formatMessage message
                msgLines' = lineWrap screenW msgText
                msgLines = statusLine : msgLines'

            page <- gets $ mcduViewPage . mcduView

            messageBindingsRaw <- messageBindings message
            let firstUsableLSKLine = ((length msgLines + 1) `div` 2) * 2
                numLSKLines = (length messageBindingsRaw `div` 2) * 2
                itemsPerPage = numLSKs * 2
                (numPagesTotal, curLines) =
                  paginateWithHeadroom (firstUsableLSKLine + numLSKLines) itemsPerPage page msgLines

            let lskTop = firstUsableLSKLine - page * itemsPerPage

            modifyView $ \v -> v { mcduViewLSKBindings = mempty }

            zipWithM_ (\n (label, action) -> do
                let n' = n + lskTop
                when (n' >= 0 && n' < numLSKs * 2) $ do
                  addLskBinding (toEnum n') (colorize white label) action
              ) [0,1..] messageBindingsRaw

            curPage <- gets $ mcduViewPage . mcduView
            when (curPage == numPagesTotal - 1) $ do
              if messageIsCPDLC message then
                addLskBinding (LSKL 4) "ATC LOG" (loadView cpdlcMessageLogView)
              else case message of
                UplinkMessage {} ->
                  addLskBinding (LSKL 4) "DLK LOG" (loadView dlkUplinkLogView)
                DownlinkMessage {} ->
                  addLskBinding (LSKL 4) "DLK LOG" (loadView dlkDownlinkLogView)


            modifyView $ \v -> v
              { mcduViewDraw = do
                  zipWithM_ (\n l -> mcduPrintColored 0 (n+1) l)
                    [0,1..] curLines
              , mcduViewNumPages = numPagesTotal
              }
    }

atcMenuView :: MCDUView
atcMenuView = defView
  { mcduViewTitle = "ATC MENU"
  , mcduViewOnLoad = do
      online <- haveCpdlcLogon
      if online then
        modifyView $ \v -> v
          { mcduViewLSKBindings = Map.fromList
              [ (LSKL 0, ("MSG LOG", loadView cpdlcMessageLogView))
              , (LSKL 1, ("LOGON", loadView cpdlcLogonView))
              , (LSKL 3, ("FREE TEXT", loadCpdlcComposeViewByID "TXTD-2" Nothing Nothing))
              , (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
              , (LSKR 0, ("EMERGENCY", return ())) -- TODO
              , (LSKR 1, ("REQUEST", loadView cpdlcRequestMenuView))
              , (LSKR 2, ("REPORT", loadView cpdlcReportMenuView))
              , (LSKR 3, ("WHEN CAN WE", loadView cpdlcWhenCanWeMenuView))
              ]
          }
      else
        modifyView $ \v -> v
          { mcduViewLSKBindings = Map.fromList
              [ (LSKL 0, ("MSG LOG", loadView cpdlcMessageLogView))
              , (LSKL 1, ("LOGON", loadView cpdlcLogonView))
              , (LSKL 5, ("MAIN MENU", loadViewByID MainMenuView))
              ]
          }
      loadUplinkLSK (LSKR 5)
  }

haveCpdlcLogon :: MCDU Bool
haveCpdlcLogon = do
  currentDA <- asks hoppieCpdlcDataAuthorities >>=
                fmap currentDataAuthority . liftIO . readMVar
  return $ isJust currentDA

cpdlcLogonView :: MCDUView
cpdlcLogonView = defView
  { mcduViewTitle = "ATC LOGON"
  , mcduViewOnLoad = do
      modifyView $ \v -> v
        { mcduViewLSKBindings = mempty }
      da <- asks hoppieCpdlcDataAuthorities >>= liftIO . readMVar

      let setLogonDA :: Maybe ByteString -> MCDU Bool
          setLogonDA ldaMay = do
            daVar <- asks hoppieCpdlcDataAuthorities
            liftIO $ modifyMVar_ daVar (\d -> return d { logonDataAuthority = ldaMay })
            return True

          getLogonDA :: MCDU (Maybe ByteString)
          getLogonDA = do
            daVar <- asks hoppieCpdlcDataAuthorities
            liftIO $ logonDataAuthority <$> readMVar daVar

      case currentDataAuthority da of
        Nothing -> do
          case nextDataAuthority da of
            Nothing -> do
              forM_ (logonDataAuthority da) $ \logonDA -> do
                addLskBinding (LSKR 0) "SEND" (lift (cpdlcLogon logonDA) >> reloadView)
              addLskBinding (LSKL 0) "" (scratchInteract setLogonDA getLogonDA >> reloadView)
            Just nextDA -> do
              addLskBinding (LSKR 1) "CANCEL LOGON" (lift (cpdlcCancelLogon nextDA) >> reloadView)
        Just currentDA -> do
          addLskBinding (LSKR 0) "LOGOFF" (lift (cpdlcPilotLogoff currentDA) >> reloadView)

      addLskBinding (LSKL 5) "ATC MENU" (loadView atcMenuView)

      modifyView $ \v -> v
        { mcduViewDraw = do
            when (isNothing (currentDataAuthority da)) $ do
              mcduPrint 1 1 white "LOGON DATA AUTHORITY"
              mcduPrint 1 2 green (fromMaybe "----" $ logonDataAuthority da)
            when (isNothing (currentDataAuthority da) && isJust (nextDataAuthority da)) $ do
              mcduPrintR (screenW - 1) 2 cyan "SENT"
            when (isJust (currentDataAuthority da)) $ do
              mcduPrintR (screenW - 1) 1 white "CONNECTED"
            forM_ (currentDataAuthority da) $ \currentDA -> do
              mcduPrint 1 3 white "CURRENT DATA AUTHORITY"
              mcduPrint 1 4 green currentDA
            forM_ (nextDataAuthority da) $ \nextDA -> do
              mcduPrint 1 5 white "NEXT DATA AUTHORITY"
              mcduPrint 1 6 green nextDA
        }
  }

cpdlcRequestMenuView :: MCDUView
cpdlcRequestMenuView = defView
  { mcduViewTitle = "ATC REQUEST"
  , mcduViewNumPages = 2
  , mcduViewOnLoad = do
      online <- haveCpdlcLogon
      if online then do
        page <- gets (mcduViewPage . mcduView)
        modifyView $ \v -> v
          { mcduViewLSKBindings = Map.fromList $
              case page of
                0 ->
                  [ (LSKL 0, ("DIRECT", loadCpdlcComposeViewByID "RTED-1" Nothing Nothing))
                  , (LSKL 1, ("LEVEL", loadCpdlcComposeViewByID "LVLD-1" Nothing Nothing))
                  , (LSKL 2, ("CLIMB", loadCpdlcComposeViewByID "LVLD-2" Nothing Nothing))
                  , (LSKL 3, ("DESCENT", loadCpdlcComposeViewByID "LVLD-3" Nothing Nothing))
                  , (LSKR 0, ("LVL AT POS", loadCpdlcComposeViewByID "LVLD-4" Nothing Nothing))
                  , (LSKR 1, ("LVL AT TIME", loadCpdlcComposeViewByID "LVLD-5" Nothing Nothing))
                  , (LSKR 2, ("SPEED", loadCpdlcComposeViewByID "SPDD-1" Nothing Nothing))
                  , (LSKR 3, ("VOICE CNT", loadCpdlcComposeViewByID "COMD-1" Nothing Nothing))

                  , (LSKL 5, ("ATC MENU", loadView atcMenuView))
                  ]
                1 ->
                  [ (LSKL 0, ("ROUTE", loadCpdlcComposeViewByID "RTED-2" Nothing Nothing))
                  , (LSKL 1, ("RTE CLX", loadCpdlcComposeViewByID "RTED-3" Nothing Nothing))
                  , (LSKL 2, ("CLEARANCE", loadCpdlcComposeViewByID "RTED-4" Nothing Nothing))
                  , (LSKL 3, ("HEADING", loadCpdlcComposeViewByID "RTED-6" Nothing Nothing))
                  , (LSKL 5, ("GROUND TRK", loadCpdlcComposeViewByID "RTED-7" Nothing Nothing))
                  , (LSKR 0, ("OFFSET", loadCpdlcComposeViewByID "LATD-1" Nothing Nothing))
                  , (LSKR 1, ("WX DEVIATION", loadCpdlcComposeViewByID "LATD-2" Nothing Nothing))
                  , (LSKR 2, ("FREE TEXT", loadCpdlcComposeViewByID "TXTD-1" Nothing Nothing))
                  ]
                _ ->
                  []
          }
        loadUplinkLSK (LSKR 5)
      else
        loadView atcMenuView
  }

cpdlcReportMenuView :: MCDUView
cpdlcReportMenuView = defView
  { mcduViewTitle = "ATC REPORT"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("POSREP", loadCpdlcComposeViewByID "RTED-5" Nothing Nothing))
      , (LSKL 1, ("ASSIGNED RTE", loadCpdlcComposeViewByID "RTED-9" Nothing Nothing))
      , (LSKL 2, ("ETA", loadCpdlcComposeViewByID "RTED-10" Nothing Nothing))
      , (LSKL 3, ("CLEAR OF WX", loadCpdlcComposeViewByID "LATD-3" Nothing Nothing))
      , (LSKL 5, ("ATC MENU", loadView atcMenuView))
      , (LSKR 0, ("BACK ON RTE", loadCpdlcComposeViewByID "LATD-4" Nothing Nothing))
      ]
  , mcduViewOnLoad = do
      online <- haveCpdlcLogon
      if online then do
        loadUplinkLSK (LSKR 5)
      else
        loadView atcMenuView
  }

cpdlcWhenCanWeMenuView :: MCDUView
cpdlcWhenCanWeMenuView = defView
  { mcduViewTitle = "ATC WHEN CAN WE"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("BACK ON RTE", loadCpdlcComposeViewByID "RTED-8" Nothing Nothing))
      , (LSKL 5, ("ATC MENU", loadView atcMenuView))
      ]
  , mcduViewOnLoad = do
      online <- haveCpdlcLogon
      if online then do
        loadUplinkLSK (LSKR 5)
      else
        loadView atcMenuView
  }

cpdlcEmergencyMenuView :: MCDUView
cpdlcEmergencyMenuView = defView
  { mcduViewTitle = "ATC EMERGENCY"
  , mcduViewLSKBindings = Map.fromList
      [ (LSKL 0, ("MAYDAY", loadCpdlcComposeViewByID "EMGD-2" Nothing Nothing))
      , (LSKL 1, ("PAN", loadCpdlcComposeViewByID "EMGD-1" Nothing Nothing))
      , (LSKR 5, ("CANCEL", loadCpdlcComposeViewByID "EMGD-4" Nothing Nothing))
      ]
  , mcduViewOnLoad = do
      online <- haveCpdlcLogon
      if online then do
        loadUplinkLSK (LSKR 5)
      else
        loadView atcMenuView
  }

