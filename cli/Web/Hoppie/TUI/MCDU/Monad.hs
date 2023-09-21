{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Monad
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.QR
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector as Vector
import Data.Word
import System.IO
import Text.Printf

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

data MCDUEvent
  = InputCommandEvent InputCommand
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  | DownlinkEvent (WithMeta DownlinkStatus TypedMessage)
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

    , mcduAircraftType :: Maybe ByteString
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
    , mcduDebugLog :: [Colored Text]
    , mcduShowLog :: Bool
    , mcduHeadless :: Bool

    , mcduHttpHostname :: Maybe String
    , mcduHttpPort :: Maybe Int
    , mcduHttpServer :: Maybe MCDUHttpServer
    }

defMCDUState :: MCDUState
defMCDUState =
  MCDUState
    { mcduScratchpad = ScratchEmpty
    , mcduScratchMessage = Nothing
    , mcduScreenBuffer = emptyMCDUScreenBuffer
    , mcduView = defView

    , mcduUnreadDLK = Nothing
    , mcduUnreadCPDLC = Nothing

    , mcduAircraftType = Nothing
    , mcduReferenceAirport = Nothing
    , mcduTelexRecipient = Nothing
    , mcduTelexBody = Nothing
    , mcduClearanceType = Nothing
    , mcduClearanceFacility = Nothing
    , mcduClearanceDestination = Nothing
    , mcduClearanceStand = Nothing
    , mcduClearanceAtis = Nothing

    , mcduSendMessage = const (return ())
    , mcduNetworkStatus = NetworkOK
    , mcduDebugLog = []
    , mcduShowLog = False
    , mcduHeadless = False

    , mcduHttpHostname = Nothing
    , mcduHttpPort = Nothing
    , mcduHttpServer = Nothing
    }

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

mcduPrintColored :: Int -> Int -> Colored ByteString -> MCDUDraw s ()
mcduPrintColored _ _ (Colored []) =
  return ()
mcduPrintColored x y (Colored (f:fs)) = do
  mcduPrint x y (cbfColor f) bs
  mcduPrintColored (x + BS.length bs) y (Colored fs)
  where
    bs = cbfData f

rawPrintColored :: PutStr a => Colored a -> IO ()
rawPrintColored (Colored []) = do
  resetFG
  resetBG
rawPrintColored (Colored (f:fs)) = do
  case cbfColor f of
    255 -> do
      resetFG
      resetBG
    254 -> do
      setFG 15
      setBG 0
    c -> do
      setFG c
      resetBG
  putString bs
  rawPrintColored (Colored fs)
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

sendMessage :: TypedMessage -> MCDU ()
sendMessage tm = do
  sendFunc <- gets mcduSendMessage
  lift $ sendFunc tm

sendInfoRequest :: ByteString -> MCDU ()
sendInfoRequest infotype = do
  gets mcduReferenceAirport >>= \case
    Nothing -> scratchWarn "INVALID"
    Just airport -> do
      sendMessage $
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
      sendMessage $
        TypedMessage Nothing to (TelexPayload body)
      return True

sendClearanceRequest :: MCDU Bool
sendClearanceRequest = do
  toBodyMay <- runMaybeT $ do
    callsign <- (lift . lift) getCallsign
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
      sendMessage $
        TypedMessage Nothing to (TelexPayload body)
      return True

sendCpdlc :: [CPDLC.MessageTypeID] -> Maybe ByteString -> Maybe Word -> Map (Int, Word) ByteString -> MCDU Bool
sendCpdlc tyIDs toMay mrnMay varDict = do
  dataAuthority <- asks hoppieCpdlcDataAuthorities >>= fmap currentDataAuthority . liftIO . readMVar
  cpdlcToMay <- runExceptT $ do
    to <- maybe (throwError "TO") return $
            toMay <|> dataAuthority

    parts <- catMaybes <$> zipWithM (\partIndex tyID -> do
        ty <- maybe (throwError $ "TYPE " <> tyID) return $
                Map.lookup tyID CPDLC.downlinkMessages
        case partIndex of
          0 -> do
                -- First part is required, all variables must be set
                argValues <- zipWithM
                  (\n _ -> maybe (throwError "ARGS") return $ Map.lookup (partIndex, n) varDict)
                  [1,2..] (CPDLC.msgArgs ty)
                return . Just $ CPDLC.CPDLCPart
                                  { cpdlcType = tyID
                                  , cpdlcArgs = argValues
                                  }
          _ -> do
                -- Other parts are supplements; we distinguish three cases:
                -- - If no variables are set, don't include the part.
                -- - If all variables are set, include it.
                -- - If some, but not all, are set, throw an error.
              let mayArgValues = zipWith (\n _ -> Map.lookup (partIndex, n) varDict) [1,2..] (CPDLC.msgArgs ty)
              if all isNothing mayArgValues then
                return Nothing
              else if all isJust mayArgValues then
                return . Just $ CPDLC.CPDLCPart
                                  { cpdlcType = tyID
                                  , cpdlcArgs = catMaybes mayArgValues
                                  }
              else
                throwError "ARGS"
                
      ) [0,1..] tyIDs
    nextMin <- lift . lift $ makeMIN
    replyOpts <- case tyIDs of
                    [] -> throwError "EMPTY MSG"
                    (tyID:_) -> do
                        ty <- maybe (throwError $ "TYPE " <> tyID) return $
                                Map.lookup tyID CPDLC.downlinkMessages
                        return $ CPDLC.msgReplyOpts ty
    let cpdlc = CPDLC.CPDLCMessage
            { cpdlcMIN = nextMin
            , cpdlcMRN = mrnMay
            , cpdlcReplyOpts = replyOpts
            , cpdlcParts = parts
            }
    return (cpdlc, to)
  case cpdlcToMay of
    Right (cpdlc, to) -> do
      sendMessage $
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
runMCDU rawSend = flip evalStateT defMCDUState { mcduSendMessage = rawSend }

draw :: (forall s. MCDUDraw s ()) -> MCDU ()
draw action = do
  modify $ \s -> s {
    mcduScreenBuffer = runMCDUDraw action (mcduScreenBuffer s)
  }

sendScreenHttp :: MCDU ()
sendScreenHttp = do
  buf <- gets mcduScreenBuffer
  serverMay <- gets mcduHttpServer
  forM_ serverMay $ \server -> do
    liftIO $ do
      void $ tryTakeMVar (mcduHttpScreenBufVar server)
      putMVar (mcduHttpScreenBufVar server) buf
      atomically $
        forM_ [0 .. screenH-1] $ \y -> do
          let l = Vector.take screenW $
                  Vector.drop (y * screenW) $
                  mcduScreenLines buf
          writeTChan (mcduHttpScreenBufChan server) $
            MCDUScreenBufferUpdate y l

unlessHeadless :: MCDU () -> MCDU ()
unlessHeadless action = do
  headless <- gets mcduHeadless
  unless headless action

flushAll :: MCDU ()
flushAll = do
  unlessHeadless $ do
    buf <- gets mcduScreenBuffer
    liftIO $ drawMCDU buf
  redrawLog
  sendScreenHttp

flushScreen :: MCDU ()
flushScreen = do
  unlessHeadless $ do
    buf <- gets mcduScreenBuffer
    liftIO $ redrawMCDU buf
  sendScreenHttp

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
  unlessHeadless $ do
    buf <- gets mcduScreenBuffer
    liftIO $ do
      redrawMCDULine (screenH - 1) buf
      moveTo 0 (screenH + 6)
      hFlush stdout
  sendScreenHttp

redrawScratch :: MCDU ()
redrawScratch = do
  (color, scratch) <- getScratchColorAndString
  let scratch' = BS.takeEnd screenW scratch
      scratch'' = scratch' <> BS.replicate (screenW - BS.length scratch') (ord8 ' ')
  draw $ mcduPrint 0 (screenH - 1) color scratch''
  flushScratch

redrawLog :: MCDU ()
redrawLog = do
  logVisible <- gets mcduShowLog
  headless <- gets mcduHeadless
  (terminalW, terminalH) <- liftIO getScreenSize
  let (left, logW) =
        if headless then
          (0, terminalW)
        else
          (screenW + 13, terminalW - screenW - 14)
  logLines <- gets $ reverse . take terminalH . concatMap (reverse . lineWrap logW) . mcduDebugLog
  liftIO $ do
    when logVisible $ do
      clearRect left 0 (terminalW - 1) (terminalH - 1)
      resetFG
      resetBG
      zipWithM_ (\y l -> liftIO $ do
          moveTo left y
          rawPrintColored (takeSubstr logW l)
          moveTo left (y + 1)
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

mcduStartHttpServer :: MCDU ()
mcduStartHttpServer = do
  portMay <- gets mcduHttpPort
  forM_ portMay $ \port -> do
    hostname <- gets (fromMaybe "localhost" . mcduHttpHostname)
    let url = Text.pack ("http://" <> hostname <> ":" <> show port)
    debugPrint $ colorize blue $
      "Starting HTTP server on " <> url
    forM_ (formatQR $ encodeUtf8 url) $
      debugPrint . colorize 254
    httpServer <- liftIO $ startHttpServer port
    modify $ \s -> s { mcduHttpServer = Just httpServer }

mcduStopHttpServer :: MCDU ()
mcduStopHttpServer = do
  serverMay <- gets mcduHttpServer
  forM_ serverMay $ \server -> do
    debugPrint $ colorize blue $ "Stopping HTTP server"
    liftIO $ stopHttpServer server
    modify $ \s -> s { mcduHttpServer = Nothing }

debugPrint :: Colored Text -> MCDU ()
debugPrint str = do
  modify $ \s -> s
    { mcduDebugLog = str : mcduDebugLog s }
  redrawLog

handleInputCommand :: MCDUView -> MCDUView -> MCDUView -> InputCommand -> MCDU ()
handleInputCommand mainMenuView dlkMenuView atcMenuView cmd =
  case cmd of
    InputPgUp ->
      prevPage
    InputPgDn ->
      nextPage

    InputF1 -> handleLSK 0
    InputF2 -> handleLSK 1
    InputF3 -> handleLSK 2
    InputF4 -> handleLSK 3
    InputF5 -> handleLSK 4
    InputF6 -> handleLSK 5
    InputF7 -> handleLSK 6
    InputF8 -> handleLSK 7
    InputF9 -> handleLSK 8
    InputF10 -> handleLSK 9
    InputF11 -> loadView dlkMenuView
    InputF12 -> loadView atcMenuView
    InputEscape -> loadView mainMenuView

    InputRedraw -> do
      flushAll
      
    InputBackspace ->
      scratchDel
    InputDel ->
      scratchClear
    InputChar c ->
      if isAsciiLower c then
        scratchAppend (ord8 $ toUpper c)
      else if isAsciiUpper c ||
              isDigit c ||
              (c `elem` ['-', '.', '/', ' ']) then
        scratchAppend (ord8 c)
      else
        return ()
    _ -> return ()

handleMCDUEvent :: MCDUView -> MCDUView -> MCDUView -> MCDUEvent -> MCDU ()
handleMCDUEvent mainMenuView dlkMenuView atcMenuView ev = do
  case ev of
    NetworkStatusEvent ns' -> do
      ns <- gets mcduNetworkStatus
      when (ns /= ns') $ do
        case ns' of
          NetworkOK -> debugPrint $ colorize green "NETWORK UP"
          NetworkError err -> debugPrint $ colorize red "NETWORK ERROR: "
                                         <> colorize 255 (Text.pack err)
        case ns' of
          NetworkError err -> do
            lift . void $ makeErrorResponse Nothing (BS8.pack err) "NETWORK ERROR"
            scratchWarn "NETWORK ERROR"
          _ ->
            return ()
      modify $ \s -> s { mcduNetworkStatus = ns' }
      reloadView
      

    DownlinkEvent mtm -> do
      debugPrint $ colorize cyan $ wordJoin
        [ "DOWNLINK"
        , Text.pack . show $ metaUID mtm
        , decodeUtf8 . typedMessageCallsign $ payload mtm
        , decodeUtf8 . typedPayloadTypeBS . typedMessagePayload $ payload mtm
        ]

    UplinkEvent mtm -> do
      debugPrint $ colorize green $ wordJoin
        [ "UPLINK"
        , Text.pack . show $ metaUID mtm
        , decodeUtf8 . typedMessageCallsign $ payload mtm
        , decodeUtf8 . typedPayloadTypeBS . typedMessagePayload $ payload mtm
        ]
      case typedMessagePayload (payload mtm) of
        CPDLCPayload {} -> do
          modify $ \s -> s { mcduUnreadCPDLC = Just (metaUID mtm) }
          scratchWarn "ATC UPLINK"
        _ -> do
          modify $ \s -> s { mcduUnreadDLK = Just (metaUID mtm) }
          scratchWarn "UPLINK"
      reloadView

    CurrentDataAuthorityEvent cda -> do
      debugPrint $ colorize 255 "Current Data Authority: "
                 <> maybe (colorize yellow "NONE") (colorize green . decodeUtf8) cda
      reloadView

    InputCommandEvent cmd -> do
      handleInputCommand mainMenuView dlkMenuView atcMenuView cmd
