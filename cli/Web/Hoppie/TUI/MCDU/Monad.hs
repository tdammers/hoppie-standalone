{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Monad
where

import Web.Hoppie.System
import Web.Hoppie.Telex
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.Input

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

data MCDUEvent
  = InputCommandEvent InputCommand
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  deriving (Show)

data MCDUState =
  MCDUState
    { mcduScratchpad :: ByteString
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
    ""
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
  let view' = view { mcduViewPage = n' }
  loadView view'

mainMenuView :: MCDUView
mainMenuView = defView
  { mcduViewTitle = "MCDU MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("DLK", loadView dlkMenuView))
      , (5, ("ATC", return ()))
      ]
  }

dlkMenuView :: MCDUView
dlkMenuView = defView
  { mcduViewTitle = "DLK MENU"
  , mcduViewLSKBindings = Map.fromList
      [ (0, ("MSG LOG", loadView dlkMessageLogView))
      , (4, ("MAIN MENU", loadView mainMenuView))
      , (5, ("ATIS", loadView atisMenuView))
      , (6, ("METAR", return ()))
      , (7, ("TAF", return ()))
      , (8, ("DLC", return ()))
      ]
  }

atisMenuView :: MCDUView
atisMenuView = defView
  { mcduViewTitle = "ATIS"
  , mcduViewLSKBindings = Map.fromList
      [ (9, ("SEND", sendAtisRequest >> reloadView ))
      , (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      refAirport <- gets mcduReferenceAirport
      debugPrint $ "Ref airport: " ++ show refAirport
      modifyView $ \v -> v {
        mcduViewLSKBindings =
          Map.insert
            5 (fromMaybe "----" refAirport, setReferenceAirport >> reloadView)
            (mcduViewLSKBindings v)
      }
  }

sendAtisRequest :: MCDU ()
sendAtisRequest = do
  sendMessage <- gets mcduSendMessage
  gets mcduReferenceAirport >>= \case
    Nothing -> scratchWarn "INVALID"
    Just airport -> do
      lift $ sendMessage $
        TypedMessage
          Nothing
          "SERVER"
          (InfoPayload $ "VATATIS " <> airport)

setReferenceAirport :: MCDU ()
setReferenceAirport = do
  val <- scratchGet
  if BS.null val then do
    refAirport <- gets mcduReferenceAirport
    forM_ refAirport scratchSet
    redrawScratch
    debugPrint $ "Cleared ref airport"
  else do
    modify $ \s -> s { mcduReferenceAirport = Just val }
    scratchClear
    refAirport <- gets mcduReferenceAirport
    debugPrint $ "Ref airport: " ++ show refAirport

dlkMessageLogView :: MCDUView
dlkMessageLogView = defView
  { mcduViewTitle = "DLK MESSAGES"
  , mcduViewLSKBindings = Map.fromList
      [ (4, ("DLK MENU", loadView dlkMenuView))
      ]
  , mcduViewOnLoad = do
      uplinks <- fmap Map.toDescList $ lift (asks hoppieUplinks) >>= liftIO . readMVar
      curPage <- gets (mcduViewPage . mcduView)
      let curUplinks = take 4 . drop (4 * curPage) $ uplinks
          numPages = (length uplinks + 3) `div` 4
      modifyView $ \s -> s
        { mcduViewNumPages = numPages
        , mcduViewLSKBindings =
            mcduViewLSKBindings s <>
              Map.fromList
                ( zip
                  [0..3]
                  [ ("", showUplink (metaUID . snd $ uplink) ) | uplink <- curUplinks ]
                )
        , mcduViewDraw = do
            zipWithM_ (\n (_, uplink) -> do
                let daySecond = floor . utctDayTime $ metaTimestamp uplink
                    (hours, minutes) = (daySecond `quot` 60 :: Int) `quotRem` 60
                    callsign = typedMessageCallsign $ payload uplink

                let (color, msgTyStr) = case typedMessagePayload $ payload uplink of
                      InfoPayload {} -> (green, "INFO" :: String)
                      TelexPayload {} -> (green, "TELEX")
                      CPDLCPayload {} -> (cyan, "CPDLC")
                      UnsupportedPayload {} -> (yellow, "UNSUPPORTED")
                      ErrorPayload {} -> (red, "ERROR")

                mcduPrint 1 (n * 2 + 1) color $
                  BS8.pack $
                  printf "%02i%02iZ %6s %s"
                    hours minutes
                    (map toUpper $ BS8.unpack callsign)
                    msgTyStr
                case typedMessagePayload $ payload uplink of
                  InfoPayload msg ->
                    mcduPrint 1 (n * 2 + 2) green msg
                  _ ->
                    mcduPrint 1 (n * 2 + 2) red "-----"
              ) [0,1..] curUplinks
        }
  }

showUplink :: Word -> MCDU ()
showUplink n = scratchWarn $ BS8.pack $ printf "UPLINK %i" n

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

flushScratch :: MCDU ()
flushScratch = do
  buf <- gets mcduScreenBuffer
  scratchX <- min (screenW - 1) . BS.length <$> gets mcduScratchpad
  let scratchY = screenH - 1
  liftIO $ do
    redrawMCDULine (screenH - 1) buf
    moveTo (screenX scratchX) (screenY scratchY)
    hFlush stdout

redrawScratch :: MCDU ()
redrawScratch = do
  warnMay <- gets mcduScratchMessage
  case warnMay of
    Nothing -> do
      scratch <- gets mcduScratchpad
      let scratch' = BS.takeEnd screenW scratch
          scratch'' = scratch' <> BS.replicate (screenW - BS.length scratch') (ord8 ' ')
      draw $ mcduPrint 0 (screenH - 1) green scratch''
    Just msg -> do
      draw $ mcduPrint 0 (screenH - 1) yellow msg
  flushScratch

modifyView :: (MCDUView -> MCDUView) -> MCDU ()
modifyView f =  do
  modify $ \s -> s { mcduView = f (mcduView s) }

modifyScratchpad :: (ByteString -> ByteString) -> MCDU ()
modifyScratchpad f =  do
  modify $ \s -> s { mcduScratchpad = f (mcduScratchpad s) }
  redrawScratch

scratchGet :: MCDU ByteString
scratchGet = do
  gets mcduScratchpad

scratchAppend :: Word8 -> MCDU ()
scratchAppend c = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad (`BS.snoc` c)
  redrawScratch

scratchSet :: ByteString -> MCDU ()
scratchSet bs = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad (const bs)
  redrawScratch

scratchClear :: MCDU ()
scratchClear = do
  gets mcduScratchMessage >>= \case
    Nothing ->
      modifyScratchpad (const "")
    Just _ ->
      modify $ \s -> s { mcduScratchMessage = Nothing }
  redrawScratch

scratchDel :: MCDU ()
scratchDel = do
  modify $ \s -> s { mcduScratchMessage = Nothing }
  modifyScratchpad (BS.dropEnd 1)
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
  mcduViewOnLoad view
  redrawView

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
    mcduPrintC (screenW `div` 2) 0 green (mcduViewTitle view)
    when (mcduViewNumPages view > 1) $ do
      let pageInfoStr = printf "%i/%i" (mcduViewPage view + 1) (mcduViewNumPages view)
      mcduPrintR screenW 0 green (BS8.pack pageInfoStr)
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
        CPDLCPayload {} ->
          scratchWarn "ATC UPLINK"
        _ ->
          scratchWarn "UPLINK"

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
