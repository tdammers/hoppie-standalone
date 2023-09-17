{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.Hoppie.TUI.MCDU.Monad
where

import Web.Hoppie.System
import Web.Hoppie.Telex
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.CPDLC.Message (renderCPDLCMessage)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Word
import System.IO
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Char
import Text.Printf
import Data.Time
import Data.Maybe
import Data.List

data MCDUEvent
  = InputCommandEvent InputCommand
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  deriving (Show)

data ScratchVal
  = ScratchEmpty
  | ScratchStr ByteString
  | ScratchDel
  deriving (Show, Read, Eq, Ord)

data MCDUState =
  MCDUState
    { mcduScratchpad :: ScratchVal
    , mcduScratchMessage :: Maybe ByteString
    , mcduScreenBuffer :: MCDUScreenBuffer
    , mcduView :: MCDUView
    , mcduUnreadDLK :: Maybe Word
    , mcduUnreadCPDLC :: Maybe Word
    , mcduReferenceAirport :: Maybe ByteString
    , mcduSendMessage :: TypedMessage -> Hoppie ()
    }

defMCDUState :: MCDUState
defMCDUState =
  MCDUState
    ScratchEmpty
    Nothing
    emptyMCDUScreenBuffer
    defView
    Nothing
    Nothing
    Nothing
    (const $ return ())

data MCDUView =
  MCDUView
    { mcduViewTitle :: ByteString
    , mcduViewLSKBindings :: Map Int (ByteString, MCDU ())
    , mcduViewDraw :: forall s. MCDUDraw s ()
    , mcduViewPage :: Int
    , mcduViewNumPages :: Int
    , mcduViewGoToPage :: Int -> MCDU ()
    , mcduViewOnLoad :: MCDU ()
    }

defView :: MCDUView
defView = MCDUView
  { mcduViewTitle = ""
  , mcduViewLSKBindings = mempty
  , mcduViewDraw = return ()
  , mcduViewPage = 0
  , mcduViewNumPages = 1
  , mcduViewGoToPage = defGoToPage
  , mcduViewOnLoad = return ()
  }

defGoToPage :: Int -> MCDU ()
defGoToPage n = do
  view <- gets mcduView
  let n' = max 0 . min (mcduViewNumPages view - 1) $ n
  modifyView $ \v -> v { mcduViewPage = n' }
  reloadView

mainMenuView :: MCDUView
mainMenuView = defView
  { mcduViewTitle = "MCDU MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("DLK", loadView dlkMenuView))
      , (5, ("ATC", return ()))
      , (3, ("TEST", loadView testView))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

testView :: MCDUView
testView = defView
  { mcduViewTitle = "TEST"
  , mcduViewDraw = do
      let lns = lineWrap screenW "THIS ATIS IS NOT AVAILABLE"
      zipWithM_ (\n l -> mcduPrint 0 (n + 1) cyan l) [0,1..] lns
  }

dlkMenuView :: MCDUView
dlkMenuView = defView
  { mcduViewTitle = "DLK MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("MSG LOG", loadView dlkMessageLogView))
      , (1, ("TELEX", return ()))
      , (4, ("MAIN MENU", loadView mainMenuView))
      , (5, ("ATIS", loadView $ infoMenuView "ATIS" "VATATIS"))
      , (6, ("METAR", loadView $ infoMenuView "METAR" "METAR"))
      , (7, ("TAF", loadView $ infoMenuView "TAF" "TAF"))
      , (8, ("DLC", return ()))
      ]
  , mcduViewOnLoad = do
      loadUplinkLSK 9
  }

infoMenuView :: ByteString -> ByteString -> MCDUView
infoMenuView title infotype = defView
  { mcduViewTitle = title
  , mcduViewLSKBindings = Map.fromList
      [ (8, ("SEND", sendInfoRequest infotype>> reloadView ))
      , (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      refAirport <- gets (fromMaybe "----" . mcduReferenceAirport)
      modifyView $ \v -> v
        { mcduViewLSKBindings =
          Map.insert
            5 ("", setReferenceAirport >> reloadView)
            (mcduViewLSKBindings v)
        , mcduViewDraw = do
            mcduPrintR (screenW - 1) (mcduLskY 5) green refAirport
        }
      loadUplinkLSK 9
  }

loadUplinkLSK :: Int -> MCDU ()
loadUplinkLSK lsk = do
  unreadDLK <- gets mcduUnreadDLK
  unreadCPDLC <- gets mcduUnreadCPDLC
  case unreadCPDLC of
    Just _cpdlcUID ->
      modifyView $ \v -> v
        { mcduViewLSKBindings =
            Map.insert lsk ("ATC UPLINK", return ()) (mcduViewLSKBindings v)
        }
    Nothing ->
      case unreadDLK of
        Just dlkUID ->
          modifyView $ \v -> v
            { mcduViewLSKBindings =
                Map.insert lsk ("DLK UPLINK", loadView $ dlkMessageView dlkUID) (mcduViewLSKBindings v)
            }
        Nothing ->
          modifyView $ \v -> v
            { mcduViewLSKBindings =
                Map.delete lsk (mcduViewLSKBindings v)
            }

sendInfoRequest :: ByteString -> MCDU ()
sendInfoRequest infotype = do
  sendMessage <- gets mcduSendMessage
  gets mcduReferenceAirport >>= \case
    Nothing -> scratchWarn "INVALID"
    Just airport -> do
      lift $ sendMessage $
        TypedMessage
          Nothing
          "SERVER"
          (InfoPayload $ infotype <> " " <> airport)

setReferenceAirport :: MCDU ()
setReferenceAirport =
  scratchInteract
    (\val -> modify $ \s -> s { mcduReferenceAirport = val })
    (gets mcduReferenceAirport)

scratchInteract :: (Maybe ByteString -> MCDU ()) -> MCDU (Maybe ByteString) -> MCDU ()
scratchInteract setVal getVal = do
  scratchGet >>= \case
    ScratchEmpty -> do
      valMay <- getVal
      forM_ valMay scratchSetStr
      redrawScratch
    ScratchStr scratchStr -> do
      setVal (Just scratchStr)
      scratchClear
    ScratchDel -> do
      setVal Nothing
      scratchClear

dlkMessageLogView :: MCDUView
dlkMessageLogView = defView
  { mcduViewTitle = "DLK MESSAGES"
  , mcduViewLSKBindings = Map.fromList
      [ (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      myCallsign <- lift $ asks hoppieCallsign
      messages <- reverse <$> lift getAllMessages
      curPage <- gets (mcduViewPage . mcduView)
      let curMessages = take 4 . drop (4 * curPage) $ messages
          numPages = (length messages + 3) `div` 4
      modifyView $ \s -> s
        { mcduViewNumPages = numPages
        , mcduViewLSKBindings =
            Map.fromList
              [ (4, ("DLK MENU", loadView dlkMenuView))
              ]
            <>
            Map.fromList
              ( zip
                [0..3]
                [ ("", loadView $ dlkMessageView (messageUID message) ) | message <- curMessages ]
              )
        , mcduViewDraw = do
            zipWithM_ (\n message -> do
                let daySecond = floor . utctDayTime $ messageTimestamp message
                    (hours, minutes) = (daySecond `quot` 60 :: Int) `quotRem` 60
                    from = messageFrom myCallsign message
                    to = messageFrom myCallsign message

                let (color', callsign, statusStr) = case message of
                      UplinkMessage m ->
                        case metaStatus m of
                          NewUplink -> (green, from, "NEW" :: String)
                          OldUplink -> (white, from, "OLD")
                      DownlinkMessage m ->
                        case metaStatus m of
                          UnsentDownlink -> (blue, to, "QUED")
                          SentDownlink -> (cyan, to, "SENT")
                          ErrorDownlink -> (red, to, "ERR")

                let (color, msgTyStr) = case typedMessagePayload $ messagePayload message of
                      InfoPayload {} -> (color', "INFO" :: String)
                      TelexPayload {} -> (color', "TELEX")
                      CPDLCPayload {} -> (color', "CPDLC")
                      UnsupportedPayload {} -> (yellow, "UNSUPPORTED")
                      ErrorPayload {} -> (red, "ERROR")

                mcduPrint 1 (n * 2 + 1) color $
                  BS8.pack $
                  printf "%02i%02iZ %-6s %-5s %-4s"
                    hours minutes
                    (map toUpper $ BS8.unpack callsign)
                    msgTyStr
                    statusStr
                case typedMessagePayload $ messagePayload message of
                  InfoPayload msg ->
                    mcduPrint 1 (n * 2 + 2) white msg
                  TelexPayload msg ->
                    mcduPrint 1 (n * 2 + 2) white msg
                  _ ->
                    mcduPrint 1 (n * 2 + 2) red "-----"
              ) [0,1..] curMessages
        }
  }

dlkMessageView :: Word -> MCDUView
dlkMessageView uid =
  defView
    { mcduViewTitle = "DLK MESSAGE"
    , mcduViewLSKBindings = Map.fromList
        [ (4, ("DLK LOG", loadView dlkMessageLogView))
        ]
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
            myCallsign <- lift $ asks hoppieCallsign
            unread <- gets mcduUnreadDLK
            when (Just (messageUID message) == unread) $ do
              modify $ \s -> s { mcduUnreadDLK = Nothing }
            case message of
              UplinkMessage {} ->
                lift $ setUplinkStatus (messageUID message) OldUplink
              DownlinkMessage {} ->
                return ()
            let daySecond = floor . utctDayTime $ messageTimestamp message
                (hours, minutes) = (daySecond `quot` 60 :: Int) `quotRem` 60
                from = messageFrom myCallsign message
                to = messageFrom myCallsign message

            let (color', callsign, statusStr) = case message of
                  UplinkMessage m ->
                    case metaStatus m of
                      NewUplink -> (green, from, "NEW" :: String)
                      OldUplink -> (white, from, "OLD")
                  DownlinkMessage m ->
                    case metaStatus m of
                      UnsentDownlink -> (blue, to, "QUED")
                      SentDownlink -> (cyan, to, "SENT")
                      ErrorDownlink -> (red, to, "ERR")

            let (color, msgTyStr, msgText) = case typedMessagePayload $ messagePayload message of
                  InfoPayload msg -> (color', "INFO" :: String, msg)
                  TelexPayload msg -> (color', "TELEX", msg)
                  CPDLCPayload cpdlc -> (color', "CPDLC", renderCPDLCMessage cpdlc)
                  UnsupportedPayload ty msg ->
                    ( yellow
                    , "UNSUPPORTED"
                    , (BS8.pack . map toUpper . show) ty <> " " <> msg
                    )
                  ErrorPayload _ response err ->
                    ( red
                    , "ERROR"
                    , (BS8.pack . map toUpper . show) err <> " " <> response
                    )
                msgLines' = lineWrap screenW msgText
                statusLine = 
                  BS8.pack $
                  printf "%02i%02iZ %-6s %-5s %-4s"
                    hours minutes
                    (map toUpper $ BS8.unpack callsign)
                    msgTyStr
                    statusStr
                msgLines = (color, statusLine) : map (white,) msgLines'
                numPages = (length msgLines + 7) `div` 8

            page <- gets $ mcduViewPage . mcduView
            let curLines = take 8 . drop (page * 8) $ msgLines
            modifyView $ \v -> v
              { mcduViewDraw = do
                  zipWithM_ (\n (c, line) -> mcduPrint 0 (n+1) c line)
                    [0,1..] curLines
              , mcduViewNumPages = numPages
              }
    }

type MCDU = StateT MCDUState Hoppie

runMCDU :: (TypedMessage -> Hoppie ()) -> MCDU a -> Hoppie a
runMCDU sendMessage = flip evalStateT defMCDUState { mcduSendMessage = sendMessage }

draw :: (forall s. MCDUDraw s ()) -> MCDU ()
draw action = do
  modify $ \s -> s {
    mcduScreenBuffer = runMCDUDraw action (mcduScreenBuffer s)
  }

flushAll :: MCDU ()
flushAll = do
  buf <- gets mcduScreenBuffer
  liftIO $ drawMCDU buf

flushScreen :: MCDU ()
flushScreen = do
  buf <- gets mcduScreenBuffer
  liftIO $ redrawMCDU buf

getScratchColorAndString :: MCDU (Word8, ByteString)
getScratchColorAndString = do
  msgMay <- gets mcduScratchMessage
  case msgMay of
    Just msg ->
      return (yellow, msg)
    Nothing ->
      gets mcduScratchpad >>= \case
        ScratchEmpty -> return (white, "")
        ScratchStr str -> return (white, str)
        ScratchDel -> return (cyan, "*DELETE*")

flushScratch :: MCDU ()
flushScratch = do
  buf <- gets mcduScreenBuffer
  (_, str) <- getScratchColorAndString
  let scratchX = min (screenW - 1) . BS.length $ str
  let scratchY = screenH - 1
  liftIO $ do
    redrawMCDULine (screenH - 1) buf
    moveTo (screenX scratchX) (screenY scratchY)
    hFlush stdout

redrawScratch :: MCDU ()
redrawScratch = do
  (color, scratch) <- getScratchColorAndString
  let scratch' = BS.takeEnd screenW scratch
      scratch'' = scratch' <> BS.replicate (screenW - BS.length scratch') (ord8 ' ')
  draw $ mcduPrint 0 (screenH - 1) color scratch''
  flushScratch

modifyView :: (MCDUView -> MCDUView) -> MCDU ()
modifyView f =  do
  modify $ \s -> s { mcduView = f (mcduView s) }

modifyScratchpad :: (ScratchVal -> ScratchVal) -> MCDU ()
modifyScratchpad f =  do
  modify $ \s -> s { mcduScratchpad = f (mcduScratchpad s) }
  redrawScratch

scratchGet :: MCDU ScratchVal
scratchGet = do
  gets mcduScratchpad

scratchAppend :: Word8 -> MCDU ()
scratchAppend c = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad $ \case
    ScratchEmpty -> ScratchStr (BS.singleton c)
    ScratchDel -> ScratchDel
    ScratchStr str -> ScratchStr (BS.snoc str c)
  redrawScratch

scratchSet :: ScratchVal -> MCDU ()
scratchSet val = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad (const val)
  redrawScratch

scratchSetStr :: ByteString -> MCDU ()
scratchSetStr bs = scratchSet (ScratchStr bs)

scratchClear :: MCDU ()
scratchClear = do
  gets mcduScratchMessage >>= \case
    Nothing ->
      modifyScratchpad $ \case
        ScratchEmpty -> ScratchDel
        _ -> ScratchEmpty
    Just _ ->
      modify $ \s -> s { mcduScratchMessage = Nothing }
  redrawScratch

scratchDel :: MCDU ()
scratchDel = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad $ \case
    ScratchStr str ->
      let str' = BS.dropEnd 1 str
      in
        if BS.null str' then
          ScratchEmpty
        else
          ScratchStr str'
    _ -> ScratchEmpty
  redrawScratch

scratchWarn :: ByteString -> MCDU ()
scratchWarn msg = do
  modify $ \s -> s { mcduScratchMessage = Just msg }
  redrawScratch

scratchClearWarn :: MCDU ()
scratchClearWarn = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  redrawScratch

loadView :: MCDUView -> MCDU ()
loadView view = do
  modify $ \s -> s { mcduView = view }
  reloadView

reloadView :: MCDU ()
reloadView = do
  view <- gets mcduView
  mcduViewOnLoad view
  redrawView

redrawView :: MCDU ()
redrawView = do
  view <- gets mcduView
  draw $ do
    mcduClearScreen
    mcduPrintC (screenW `div` 2) 0 white (mcduViewTitle view)
    when (mcduViewNumPages view > 1) $ do
      let pageInfoStr = printf "%i/%i" (mcduViewPage view + 1) (mcduViewNumPages view)
      mcduPrintR screenW 0 white (BS8.pack pageInfoStr)
    mcduViewDraw view
    forM_ (Map.toList $ mcduViewLSKBindings view) $ \(n, (label, _)) -> do
      if n `div` 5 == 0 then
        mcduPrintLskL n label
      else
        mcduPrintLskR n label
  redrawScratch
  flushScreen

nextPage :: MCDU ()
nextPage = do
  view <- gets mcduView
  mcduViewGoToPage view (mcduViewPage view + 1)

prevPage :: MCDU ()
prevPage = do
  view <- gets mcduView
  mcduViewGoToPage view (mcduViewPage view - 1)

handleLSK :: Int -> MCDU ()
handleLSK n = do
  view <- gets mcduView
  let bindingMay = Map.lookup n (mcduViewLSKBindings view)
  case bindingMay of
    Nothing -> return ()
    Just (_, action) -> action

debugPrint :: String -> MCDU ()
debugPrint str =
  liftIO $ do
    resetFG
    resetBG
    moveTo 0 (screenH + 7)
    putStrLn $ centerTo (screenW + 12) str

handleMCDUEvent :: MCDUEvent -> MCDU ()
handleMCDUEvent ev = do
  liftIO $ do
    resetFG
    resetBG
    moveTo 0 (screenH + 6)
    putStrLn $ centerTo (screenW + 12) (show ev)
  case ev of
    UplinkEvent mtm -> do
      case typedMessagePayload (payload mtm) of
        CPDLCPayload {} -> do
          modify $ \s -> s { mcduUnreadCPDLC = Just (metaUID mtm) }
          scratchWarn "ATC UPLINK"
        _ -> do
          modify $ \s -> s { mcduUnreadDLK = Just (metaUID mtm) }
          scratchWarn "UPLINK"
      reloadView

    InputCommandEvent InputPgUp ->
      prevPage
    InputCommandEvent InputPgDn ->
      nextPage

    InputCommandEvent InputF1 -> handleLSK 0
    InputCommandEvent InputF2 -> handleLSK 1
    InputCommandEvent InputF3 -> handleLSK 2
    InputCommandEvent InputF4 -> handleLSK 3
    InputCommandEvent InputF5 -> handleLSK 4
    InputCommandEvent InputF6 -> handleLSK 5
    InputCommandEvent InputF7 -> handleLSK 6
    InputCommandEvent InputF8 -> handleLSK 7
    InputCommandEvent InputF9 -> handleLSK 8
    InputCommandEvent InputF10 -> handleLSK 9
    InputCommandEvent InputF11 -> loadView dlkMenuView
    -- InputCommandEvent InputF12 -> loadView atcMenuView
    InputCommandEvent InputEscape -> loadView mainMenuView
      
    InputCommandEvent InputBackspace ->
      scratchDel
    InputCommandEvent InputDel ->
      scratchClear
    InputCommandEvent (InputChar c) ->
      if c >= 'a' && c <= 'z' then
        scratchAppend (ord8 $ toUpper c)
      else if (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9') ||
              (c `elem` ['-', '.', '/', ' ']) then
        scratchAppend (ord8 c)
      else
        return ()
    _ -> return ()
