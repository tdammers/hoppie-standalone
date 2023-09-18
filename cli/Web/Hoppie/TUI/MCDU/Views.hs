{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.Hoppie.TUI.MCDU.Views
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
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
import Text.Printf
import Data.Word

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

loadUplinkLSK :: Int -> MCDU ()
loadUplinkLSK lsk = do
  unreadDLK <- gets mcduUnreadDLK
  unreadCPDLC <- gets mcduUnreadCPDLC
  case unreadCPDLC of
    Just cpdlcUID ->
      addLskBinding lsk "ATC UPLINK" $
        loadView (messageView cpdlcUID)
    Nothing ->
      case unreadDLK of
        Just dlkUID ->
          addLskBinding lsk "DLK UPLINK" $
            loadView (messageView dlkUID)
        Nothing ->
          removeLskBinding lsk


mainMenuView :: MCDUView
mainMenuView = defView
  { mcduViewTitle = "MCDU MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("DLK", loadView dlkMenuView))
      , (5, ("ATC", loadView atcMenuView))
      , (3, ("TEST", loadView testView))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

testView :: MCDUView
testView = defView
  { mcduViewTitle = "TEST"
  , mcduViewDraw = do
      let lns = lineWrap screenW $
                  ColoredBS
                    [ ColoredBSFragment white "THIS ATIS "
                    , ColoredBSFragment red " IS NOT AVAILABLE"
                    ]
      zipWithM_ (\n cbs -> mcduPrintColored 0 (n + 1) cbs) [0,1..] lns
  }

dlkMenuView :: MCDUView
dlkMenuView = defView
  { mcduViewTitle = "DLK MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("MSG LOG", loadView dlkMessageLogView))
      , (1, ("TELEX", clearTelexBody >> loadView (telexSendView False)))
      , (4, ("MAIN MENU", loadView mainMenuView))
      , (5, ("ATIS", loadView $ infoMenuView "ATIS" "VATATIS"))
      , (6, ("METAR", loadView $ infoMenuView "METAR" "METAR"))
      , (7, ("TAF", loadView $ infoMenuView "TAF" "TAF"))
      , (8, ("DCL", loadView (clearanceSendView False)))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

data CPDLCEditorState =
  CPDLCEditorState
    { cpdlcEditorVars :: Map Word ByteString
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
  let getVar index = Map.lookup index . cpdlcEditorVars <$> liftIO (readMVar editorStateVar)
      setVar index (Just val) = do
        liftIO $
          modifyMVar_ editorStateVar (\s -> return s
            { cpdlcEditorVars = Map.insert index val (cpdlcEditorVars s) })
        return True
      setVar index Nothing = do
        liftIO $
          modifyMVar_ editorStateVar (\s -> return s
            { cpdlcEditorVars = Map.delete index (cpdlcEditorVars s) })
        return True
      entrySpec = getEntryItems ty
      hydrate = do
        vars <- cpdlcEditorVars <$> liftIO (readMVar editorStateVar)
        return [ (label, varMay, varMay >>= \i -> Map.lookup i vars)
               | (label, varMay) <- entrySpec
               ]
  let numPages = (length entrySpec + 5) `div` 5
  return defView
            { mcduViewTitle = "CPDLC COMPOSE"
            , mcduViewNumPages = numPages
            , mcduViewOnLoad = do
                entryItems <- hydrate
                curPage <- gets (mcduViewPage . mcduView)
                let curEntryItems = take 5 . drop (curPage * 5) $ entryItems
                    lastPage = curPage == numPages - 1
                modifyView $ \v -> v
                    { mcduViewLSKBindings = mempty
                    , mcduViewDraw = do
                        zipWithM_ (\y (label, varMay, valMay) -> do
                            mcduPrint 1 (y * 2 + 1) white label
                            forM_ varMay $ \_var -> do
                              let valFmt = fromMaybe "----" valMay
                              mcduPrintR (screenW - 1) (y * 2 + 2) green valFmt
                          )
                          [0..4] curEntryItems
                    }
                let mkBinding n (_, varMay, _) = do
                      forM_ varMay $ \index -> do
                        addLskBinding (n + 5) "" $ do
                          scratchInteract
                            (setVar index)
                            (getVar index)
                          reloadView
                zipWithM_ mkBinding [0..4] curEntryItems
                when lastPage $ do
                  vars <- liftIO $ cpdlcEditorVars <$> readMVar editorStateVar
                  addLskBinding 8 "SEND"
                    (sendCpdlc tyID toMay mrnMay vars >>= \case
                      True -> loadView cpdlcMessageLogView
                      False -> reloadView
                    )
                  loadUplinkLSK 9
            }

getEntryItems :: CPDLC.MessageType -> [(ByteString, Maybe Word)]
getEntryItems ty =
  go (CPDLC.messagePatternItems $ CPDLC.msgPattern ty)
  where
    go :: [CPDLC.MessagePatternItem Word] -> [(ByteString, Maybe Word)]
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
          ([], (CPDLC.MessageArg i:xs')) ->
            ("", Just i) : go xs'
          (_, (CPDLC.MessageArg i:xs')) ->
            [(lit, Nothing) | lit <- init literalLines] ++
            [(lit, Just i) | lit <- drop (length literalLines - 1) literalLines] ++
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
      [ (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      callsign <- asks hoppieCallsign
      unless sent $ do
        addLskBinding 8 "SEND" $ do
          sendSuccess <- sendTelex
          when sendSuccess $ do
            loadView (telexSendView True)

      addLskBinding 5 "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduTelexRecipient = val }))
          (gets mcduTelexRecipient)
        reloadView

      addLskBinding 1 "" $ do
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
            [0..4] bodyLines
      }
      loadUplinkLSK 9
  }

clearanceSendView :: Bool -> MCDUView
clearanceSendView sent = defView
  { mcduViewTitle = "DATALINK CLEARANCE"
  , mcduViewLSKBindings = Map.fromList
      [ (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      callsign <- asks hoppieCallsign
      unless sent $ do
        addLskBinding 8 "SEND" $ do
          sendSuccess <- sendClearanceRequest
          when sendSuccess $ do
            loadView (clearanceSendView True)

      addLskBinding 1 "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceType = val }))
          (gets mcduClearanceType)
        reloadView

      addLskBinding 2 "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceStand = val }))
          (gets mcduClearanceStand)
        reloadView

      addLskBinding 3 "" $ do
        scratchInteract 
          (\case
              Nothing -> True <$ (modify $ \s -> s { mcduClearanceAtis = Nothing })
              Just val -> case BS.unpack val of
                [letter] -> True <$ (modify $ \s -> s { mcduClearanceAtis = Just letter })
                _ -> return False
          )
          (fmap BS.singleton <$> gets mcduClearanceAtis)
        reloadView

      addLskBinding 5 "" $ do
        scratchInteract 
          (\val -> True <$ (modify $ \s -> s { mcduClearanceFacility = val }))
          (gets mcduClearanceFacility)
        reloadView

      addLskBinding 6 "" $ do
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
      loadUplinkLSK 9
  }

infoMenuView :: ByteString -> ByteString -> MCDUView
infoMenuView title infotype = defView
  { mcduViewTitle = title
  , mcduViewLSKBindings = Map.fromList
      [ (8, ("SEND", sendInfoRequest infotype >> reloadView ))
      , (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      refAirport <- gets (fromMaybe "----" . mcduReferenceAirport)
      addLskBinding 5 "" (setReferenceAirport >> reloadView)
      modifyView $ \v -> v
        { mcduViewDraw =
            mcduPrintR (screenW - 1) (mcduLskY 5) green refAirport
        }
      loadUplinkLSK 9
  }

dlkMessageLogView :: MCDUView
dlkMessageLogView = messageLogView False

cpdlcMessageLogView :: MCDUView
cpdlcMessageLogView = messageLogView True

messageLogView :: Bool -> MCDUView
messageLogView cpdlcView = defView
  { mcduViewTitle = if cpdlcView then "ATC MESSAGES" else "DLK MESSAGES"
  , mcduViewLSKBindings = Map.fromList
      [ (4, if cpdlcView then ("ATC MENU", loadView atcMenuView) else ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      messages <- reverse . filter (if cpdlcView then messageIsCPDLC else not . messageIsCPDLC)
                    <$> lift getAllMessages
      curPage <- gets (mcduViewPage . mcduView)
      let curMessages = take 4 . drop (4 * curPage) $ messages
          numPages = (length messages + 3) `div` 4
      modifyView $ \s -> s
        { mcduViewNumPages = numPages
        , mcduViewLSKBindings =
            Map.fromList
              [ (4, if cpdlcView then ("ATC MENU", loadView atcMenuView) else ("DLK MENU", loadView dlkMenuView))
              ]
            <>
            Map.fromList
              ( zip
                [0..3]
                [ ("", loadView $
                    -- (if cpdlcView then cpdlcMessageView else messageView)
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

formatCPDLC :: CPDLCMessage -> ColoredBS
formatCPDLC =
  lineJoin . map formatCPDLCPart . cpdlcParts

formatCPDLCPart :: CPDLCPart -> ColoredBS
formatCPDLCPart part =
  maybe
    (ColoredBS [ColoredBSFragment red "INVALID CPDLC"])
    goTy
    tyMay
  where
    tyMay = Map.lookup (cpdlcType part) CPDLC.allMessageTypes

    argsColored =
      map (colorize green) (CPDLC.cpdlcArgs part)
      ++
      (repeat $ ColoredBS [ColoredBSFragment red "N/A"])


    goTy :: CPDLC.MessageType -> ColoredBS
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

messageColorTyStrBody :: Word8 -> HoppieMessage -> (Word8, String, ColoredBS)
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

formatMessage :: HoppieMessage -> (ColoredBS, ColoredBS)
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

messageBindings :: HoppieMessage -> [(ByteString, MCDU ())]
messageBindings (UplinkMessage mtm) =
  case typedMessagePayload . payload $ mtm of
    CPDLCPayload cpdlc ->
      let mrn = CPDLC.cpdlcMIN cpdlc
          sender = typedMessageCallsign . payload $ mtm
      in
        case CPDLC.cpdlcParts cpdlc of
          [] -> []
          (part:_) ->
            let tyMay = Map.lookup (CPDLC.cpdlcType part) CPDLC.allMessageTypes
            in case tyMay of
              Nothing -> []
              Just ty -> case CPDLC.msgReplyOpts ty of
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
    _ -> []
messageBindings _ = []

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
                -- numPages = (length msgLines + 9) `div` 10

            page <- gets $ mcduViewPage . mcduView

            let messageBindingsRaw = messageBindings message
                firstUsableLSKLine = ((length msgLines + 1) `div` 2) * 2
                numLSKLines = (length messageBindingsRaw `div` 2) * 2
                numPagesTotal = (firstUsableLSKLine + numLSKLines + 10) `div` 10

            let curLines = take 10 . drop (page * 10) $ msgLines

            let lskTop = firstUsableLSKLine - page * 10

            modifyView $ \v -> v { mcduViewLSKBindings = mempty }

            zipWithM_ (\n (label, action) -> do
                let n' = n + lskTop
                let lsk = (n' `div` 2) + (n' `mod` 2) * 5
                when (lsk >= 0 && lsk < 10) $ do
                  addLskBinding lsk label action
              ) [0,1..] messageBindingsRaw

            curPage <- gets $ mcduViewPage . mcduView
            when (curPage == numPagesTotal - 1) $ do
              if messageIsCPDLC message then
                addLskBinding 4 "ATC LOG" (loadView cpdlcMessageLogView)
              else
                addLskBinding 4 "DLK LOG" (loadView dlkMessageLogView)


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
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("MSG LOG", loadView cpdlcMessageLogView))
      , (1, ("LOGON", return ())) -- TODO
      , (3, ("FREE TEXT", loadCpdlcComposeViewByID "TXTD-2" Nothing Nothing))
      , (4, ("MAIN MENU", loadView mainMenuView))
      , (5, ("EMERGENCY", return ())) -- TODO
      , (6, ("REQUEST", loadView cpdlcRequestMenuView))
      , (7, ("REPORT", loadView cpdlcReportMenuView))
      , (8, ("WHEN CAN WE", loadView cpdlcWhenCanWeMenuView))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

cpdlcRequestMenuView :: MCDUView
cpdlcRequestMenuView = defView
  { mcduViewTitle = "ATC REQUEST"
  , mcduViewNumPages = 2
  , mcduViewOnLoad = do
      page <- gets (mcduViewPage . mcduView)
      modifyView $ \v -> v
        { mcduViewLSKBindings = Map.fromList $
            case page of
              0 ->
                [ (0, ("DIRECT", loadCpdlcComposeViewByID "RTED-1" Nothing Nothing))
                , (1, ("LEVEL", loadCpdlcComposeViewByID "LVLD-1" Nothing Nothing))
                , (2, ("CLIMB", loadCpdlcComposeViewByID "LVLD-2" Nothing Nothing))
                , (3, ("DESCENT", loadCpdlcComposeViewByID "LVLD-3" Nothing Nothing))
                , (5, ("LVL AT POS", loadCpdlcComposeViewByID "LVLD-4" Nothing Nothing))
                , (6, ("LVL AT TIME", loadCpdlcComposeViewByID "LVLD-5" Nothing Nothing))
                , (7, ("SPEED", loadCpdlcComposeViewByID "SPDD-1" Nothing Nothing))
                , (8, ("VOICE CNT", loadCpdlcComposeViewByID "COMD-1" Nothing Nothing))

                , (4, ("ATC MENU", loadView atcMenuView))
                ]
              1 ->
                [ (0, ("ROUTE", loadCpdlcComposeViewByID "RTED-2" Nothing Nothing))
                , (1, ("RTE CLX", loadCpdlcComposeViewByID "RTED-3" Nothing Nothing))
                , (2, ("CLEARANCE", loadCpdlcComposeViewByID "RTED-4" Nothing Nothing))
                , (3, ("HEADING", loadCpdlcComposeViewByID "RTED-6" Nothing Nothing))
                , (4, ("GROUND TRK", loadCpdlcComposeViewByID "RTED-7" Nothing Nothing))
                , (5, ("OFFSET", loadCpdlcComposeViewByID "LATD-1" Nothing Nothing))
                , (6, ("WX DEVIATION", loadCpdlcComposeViewByID "LATD-2" Nothing Nothing))
                , (7, ("FREE TEXT", loadCpdlcComposeViewByID "TXTD-1" Nothing Nothing))
                ]
              _ ->
                []
        }
      loadUplinkLSK 9
  }

cpdlcReportMenuView :: MCDUView
cpdlcReportMenuView = defView
  { mcduViewTitle = "ATC REPORT"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("POSREP", loadCpdlcComposeViewByID "RTED-5" Nothing Nothing))
      , (1, ("ASSIGNED RTE", loadCpdlcComposeViewByID "RTED-9" Nothing Nothing))
      , (2, ("ETA", loadCpdlcComposeViewByID "RTED-10" Nothing Nothing))
      , (3, ("CLEAR OF WX", loadCpdlcComposeViewByID "LATD-3" Nothing Nothing))
      , (4, ("ATC MENU", loadView atcMenuView))
      , (5, ("BACK ON RTE", loadCpdlcComposeViewByID "LATD-4" Nothing Nothing))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

cpdlcWhenCanWeMenuView :: MCDUView
cpdlcWhenCanWeMenuView = defView
  { mcduViewTitle = "ATC WHEN CAN WE"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("BACK ON RTE", loadCpdlcComposeViewByID "RTED-8" Nothing Nothing))
      , (4, ("ATC MENU", loadView atcMenuView))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

