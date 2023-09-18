{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.Hoppie.TUI.MCDU.Monad
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time
import Data.Word
import System.IO
import Text.Printf

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

data MCDUEvent
  = InputCommandEvent InputCommand
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  | NetworkStatusEvent NetworkStatus
  | CurrentDataAuthorityEvent (Maybe ByteString)
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
    , mcduTelexRecipient :: Maybe ByteString
    , mcduTelexBody :: Maybe ByteString
    , mcduClearanceType :: Maybe ByteString
    , mcduClearanceFacility :: Maybe ByteString
    , mcduClearanceDestination :: Maybe ByteString
    , mcduClearanceStand :: Maybe ByteString
    , mcduClearanceAtis :: Maybe Word8
    , mcduSendMessage :: TypedMessage -> Hoppie ()
    , mcduNetworkStatus :: NetworkStatus
    , mcduDebugLog :: [String]
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
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    (const $ return ())
    NetworkOK
    []

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

mcduPrintColored :: Int -> Int -> ColoredBS -> MCDUDraw s ()
mcduPrintColored _ _ (ColoredBS []) =
  return ()
mcduPrintColored x y (ColoredBS (f:fs)) = do
  mcduPrint x y (cbfColor f) bs
  mcduPrintColored (x + BS.length bs) y (ColoredBS fs)
  where
    bs = cbfData f
  
clearTelexBody :: MCDU ()
clearTelexBody =
  modify $ \s -> s
    { mcduTelexBody = Nothing }

addLskBinding :: Int -> ByteString -> MCDU () -> MCDU ()
addLskBinding lsk label action =
  modifyView $ \v -> v {
    mcduViewLSKBindings =
      Map.insert lsk (label, action) (mcduViewLSKBindings v)
  }

removeLskBinding :: Int -> MCDU ()
removeLskBinding lsk =
  modifyView $ \v -> v {
    mcduViewLSKBindings =
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

sendTelex :: MCDU Bool
sendTelex = do
  toMay <- gets mcduTelexRecipient
  bodyMay <- gets mcduTelexBody
  case (toMay, bodyMay) of
    (Nothing, _) -> do
      scratchWarn "NO RECIPIENT"
      return False
    (_, Nothing) -> do
      scratchWarn "NO MESSAGE"
      return False
    (Just to, Just body) -> do
      sendMessage <- gets mcduSendMessage
      lift $ sendMessage $
        TypedMessage Nothing to (TelexPayload body)
      return True

sendClearanceRequest :: MCDU Bool
sendClearanceRequest = do
  toBodyMay <- runMaybeT $ do
    callsign <- asks hoppieCallsign
    clearanceType <- MaybeT $ gets mcduClearanceType
    clearanceFacility <- MaybeT $ gets mcduClearanceFacility
    clearanceDestination <- MaybeT $ gets mcduClearanceDestination
    clearanceStand <- MaybeT $ gets mcduClearanceStand
    clearanceAtis <- MaybeT $ gets mcduClearanceAtis
    let body =
          mconcat
            [ "REQUEST PREDEP CLEARANCE "
            , callsign
            , " "
            , clearanceType
            , " TO "
            , clearanceDestination
            , " AT "
            , clearanceFacility
            , " STAND "
            , clearanceStand
            , " ATIS "
            , BS.singleton clearanceAtis
            ]
    return (clearanceFacility, body)
  case toBodyMay of
    Nothing -> do
      scratchWarn "INVALID"
      return False
    Just (to, body) -> do
      sendMessage <- gets mcduSendMessage
      lift $ sendMessage $
        TypedMessage Nothing to (TelexPayload body)
      return True

sendCpdlc :: CPDLC.MessageTypeID -> Maybe ByteString -> Maybe Word -> Map Word ByteString -> MCDU Bool
sendCpdlc tyID toMay mrnMay varDict = do
  sendMessage <- gets mcduSendMessage
  dataAuthority <- asks hoppieCpdlcDataAuthorities >>= fmap currentDataAuthority . liftIO . readMVar
  cpdlcToMay <- runExceptT $ do
    ty <- ExceptT . return . maybe (Left "TYPE") Right $ Map.lookup tyID CPDLC.downlinkMessages
    to <- ExceptT . return . maybe (Left "TO") Right $ toMay <|> dataAuthority
    argValues <- zipWithM
          (\n _ -> ExceptT . return . maybe (Left "ARGS") Right $ Map.lookup n varDict)
          [1,2..] (CPDLC.msgArgs ty)
    nextMin <- lift . lift $ makeMIN
    let cpdlc = CPDLC.CPDLCMessage
            { cpdlcMIN = nextMin
            , cpdlcMRN = mrnMay
            , cpdlcReplyOpts = CPDLC.msgReplyOpts ty
            , cpdlcParts =
                [ CPDLC.CPDLCPart
                    { cpdlcType = tyID
                    , cpdlcArgs = argValues
                    }
                ]
            }
    return (cpdlc, to)
  case cpdlcToMay of
    Right (cpdlc, to) -> do
      lift $ sendMessage $
        TypedMessage Nothing to (CPDLCPayload cpdlc)
      return True
    Left err-> do
      scratchWarn $ "INVALID " <> err
      return False

setReferenceAirport :: MCDU ()
setReferenceAirport =
  scratchInteract
    (\val -> True <$ modify (\s -> s { mcduReferenceAirport = val }))
    (gets mcduReferenceAirport)

scratchInteract :: (Maybe ByteString -> MCDU Bool) -> MCDU (Maybe ByteString) -> MCDU ()
scratchInteract setVal getVal = do
  scratchGet >>= \case
    ScratchEmpty -> do
      valMay <- getVal
      forM_ valMay scratchSetStr
      redrawScratch
    ScratchStr scratchStr -> do
      setVal (Just scratchStr) >>= flip when scratchClear
    ScratchDel -> do
      setVal Nothing >>= flip when scratchClear

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
  redrawLog

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

redrawLog :: MCDU ()
redrawLog = do
  (terminalW, terminalH) <- liftIO $ getScreenSize
  let logW = terminalW - screenW - 14
  logLines <- gets $ reverse . take terminalH . concat . map (reverse . lineWrap logW) . mcduDebugLog
  liftIO $ do
    clearRect (screenW + 13) 0 (terminalW - 1) (terminalH - 1)
    resetFG
    resetBG
    zipWithM_ (\y l -> liftIO $ do
        moveTo (screenW + 13) y
        putStr (take logW l)
      ) [0..] logLines
    hFlush stdout

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
debugPrint str = do
  modify $ \s -> s
    { mcduDebugLog = str : mcduDebugLog s }
  redrawLog

handleMCDUEvent :: MCDUView -> MCDUView -> MCDUView -> MCDUEvent -> MCDU ()
handleMCDUEvent mainMenuView dlkMenuView atcMenuView ev = do
  case ev of
    NetworkStatusEvent ns' -> do
      ns <- gets mcduNetworkStatus
      when (ns /= ns') $ do
        debugPrint (show ns')
        case ns' of
          NetworkError err -> do
            lift . void $ makeErrorResponse Nothing (BS8.pack err) "NETWORK ERROR"
            scratchWarn "NETWORK ERROR"
          _ ->
            return ()
      modify $ \s -> s { mcduNetworkStatus = ns' }
      

    UplinkEvent mtm -> do
      debugPrint (show mtm)
      case typedMessagePayload (payload mtm) of
        CPDLCPayload {} -> do
          modify $ \s -> s { mcduUnreadCPDLC = Just (metaUID mtm) }
          scratchWarn "ATC UPLINK"
        _ -> do
          modify $ \s -> s { mcduUnreadDLK = Just (metaUID mtm) }
          scratchWarn "UPLINK"
      reloadView

    CurrentDataAuthorityEvent cda -> do
      debugPrint (show cda)
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
    InputCommandEvent InputF12 -> loadView atcMenuView
    InputCommandEvent InputEscape -> loadView mainMenuView

    InputCommandEvent InputRedraw -> do
      flushAll
      
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
