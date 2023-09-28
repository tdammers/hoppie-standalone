{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Web.Hoppie.TUI.MCDU.Monad
( module Web.Hoppie.TUI.MCDU.Monad
, module Web.Hoppie.TUI.MCDU.Keys
)
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.MCDU.Keys
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.QR
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex
import Web.Hoppie.TUI.MCDU.Views.Enum

import Control.Concurrent
import Control.Applicative
import Control.Concurrent.Async
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
import Control.Exception

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

data MCDUEvent
  = KeyEvent MCDUKey
  | RedrawEvent
  | UplinkEvent (WithMeta UplinkStatus TypedMessage)
  | DownlinkEvent (WithMeta DownlinkStatus TypedMessage)
  | NetworkStatusEvent NetworkStatus
  | CurrentDataAuthorityEvent (Maybe ByteString)
  | LogEvent Text
  | TickEvent
  | FGFSConnectEvent FGFSConnection
  | FGFSDisconnectEvent
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
    , mcduResolveViewID :: ViewID -> MCDUView

    , mcduEventChan :: Maybe (TChan MCDUEvent)

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

    , mcduFlightgearConnection :: Maybe FGFSConnection
    , mcduFlightgearThread :: Maybe (Async ())
    , mcduFlightgearHostname :: Maybe String
    , mcduFlightgearPort :: Maybe Int
    }

defMCDUState :: MCDUState
defMCDUState =
  MCDUState
    { mcduScratchpad = ScratchEmpty
    , mcduScratchMessage = Nothing
    , mcduScreenBuffer = emptyMCDUScreenBuffer
    , mcduView = defView
    , mcduResolveViewID = const defView

    , mcduEventChan = Nothing

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

    , mcduFlightgearConnection = Nothing
    , mcduFlightgearThread = Nothing
    , mcduFlightgearHostname = Nothing
    , mcduFlightgearPort = Nothing
    }

data MCDUView =
  MCDUView
    { mcduViewTitle :: ByteString
    , mcduViewLSKBindings :: Map LSK (Colored ByteString, MCDU ())
    , mcduViewDraw :: forall s. MCDUDraw s ()
    , mcduViewPage :: Int
    , mcduViewNumPages :: Int
    , mcduViewGoToPage :: Int -> MCDU ()
    , mcduViewOnLoad :: MCDU ()
    , mcduViewOnUnload :: MCDU ()
    , mcduViewAutoReload :: Bool
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
  , mcduViewOnUnload = return ()
  , mcduViewAutoReload = False
  }

defGoToPage :: Int -> MCDU ()
defGoToPage n = do
  view <- gets mcduView
  let n' = max 0 . min (mcduViewNumPages view - 1) $ n
  modifyView $ \v -> v { mcduViewPage = n' }
  reloadView

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

addLskBinding :: LSK -> Colored ByteString -> MCDU () -> MCDU ()
addLskBinding lsk label action =
  modifyView $ \v -> v {
    mcduViewLSKBindings =
      Map.insert lsk (label, action) (mcduViewLSKBindings v)
  }

removeLskBinding :: LSK -> MCDU ()
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

scratchInteractOrSelect :: MCDU () -> (Maybe ByteString -> MCDU Bool) -> MCDU ()
scratchInteractOrSelect select setVal = do
  scratchGet >>= \case
    ScratchEmpty -> do
      select
    ScratchStr scratchStr -> do
      setVal (Just scratchStr) >>= flip when scratchClear
    ScratchDel -> do
      setVal Nothing >>= flip when scratchClear

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

mcduCatch :: Exception e => MCDU a -> (e -> MCDU a) -> MCDU a
mcduCatch action handler = do
  state0 <- get
  env <- lift ask
  (result, state') <- liftIO $
    (runReaderT (runStateT action state0) env)
    `catch`
    (\e -> runReaderT (runStateT (handler e) state0) env)
  put state'
  return result

data MCDUHandler a = forall e. Exception e => MCDUHandler (e -> MCDU a)

mcduCatches :: MCDU a -> [MCDUHandler a] -> MCDU a
mcduCatches action handlers = do
  state0 <- get
  env <- lift ask
  (result, state') <- liftIO $
    (runReaderT (runStateT action state0) env)
    `catches`
    [ Handler (\e -> runReaderT (runStateT (handler e) state0) env)
    | MCDUHandler handler <- handlers
    ]
  put state'
  return result

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
  headless <- gets mcduHeadless
  liftIO $ do
    clearScreen
    moveTo 0 0
  if headless then do
    liftIO $ putStrLn "HEADLESS MODE"
  else do
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
          (screenW + 15, terminalW - screenW - 15)
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

loadViewByID :: ViewID -> MCDU ()
loadViewByID viewID = do
  loadView =<< resolveViewID viewID

resolveViewID :: ViewID -> MCDU MCDUView
resolveViewID viewID = do
  resolver <- gets mcduResolveViewID
  return $ resolver viewID

loadView :: MCDUView -> MCDU ()
loadView view = do
  join $ gets (mcduViewOnUnload . mcduView)
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
      case n of
        LSKL i ->
          mcduPrintLskL i label
        LSKR i ->
          mcduPrintLskR i label
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

handleLSK :: LSK -> MCDU ()
handleLSK n = do
  view <- gets mcduView
  let bindingMay = Map.lookup n (mcduViewLSKBindings view)
  case bindingMay of
    Nothing -> return ()
    Just (_, action) -> action

mcduPrintHttpServerQR :: MCDU ()
mcduPrintHttpServerQR = do
  portMay <- gets mcduHttpPort
  forM_ portMay $ \port -> do
    hostnameMay <- gets mcduHttpHostname
    mcduPrintHttpServerQRWith port hostnameMay

mcduPrintHttpServerQRWith :: Int -> Maybe String -> MCDU ()
mcduPrintHttpServerQRWith port hostnameMay = do
  let hostname = fromMaybe "localhost" hostnameMay
  let url = Text.pack ("http://" <> hostname <> ":" <> show port)
  forM_ (formatQR $ encodeUtf8 url) $
    debugPrint . colorize 254

mcduStartHttpServer :: MCDU ()
mcduStartHttpServer = do
  portMay <- gets mcduHttpPort
  forM_ portMay $ \port -> do
    hostname <- gets (fromMaybe "localhost" . mcduHttpHostname)
    let url = Text.pack ("http://" <> hostname <> ":" <> show port)
    debugPrint $ colorize blue $
      "Starting HTTP server on " <> url
    mcduPrintHttpServerQRWith port (Just hostname)
    httpServer <- liftIO $ startHttpServer port
    modify $ \s -> s { mcduHttpServer = Just httpServer }

mcduStopHttpServer :: MCDU ()
mcduStopHttpServer = do
  serverMay <- gets mcduHttpServer
  forM_ serverMay $ \server -> do
    debugPrint $ colorize blue $ "Stopping HTTP server"
    liftIO $ stopHttpServer server
    modify $ \s -> s { mcduHttpServer = Nothing }

mcduConnectFlightgear :: MCDU ()
mcduConnectFlightgear = mcduConnectFlightgearAfter 0

mcduConnectFlightgearAfter :: Int -> MCDU ()
mcduConnectFlightgearAfter time = do
  eventChan <- gets $ fromMaybe (error "Event chan not set up") . mcduEventChan
  fgthread <- gets mcduFlightgearThread
  fghostMay <- gets mcduFlightgearHostname
  fgportMay <- gets mcduFlightgearPort
  case (fgthread, fghostMay, fgportMay) of
    (Nothing, Just fghost, Just fgport) -> do
      connVar <- liftIO newEmptyMVar
      fgfsThread <- liftIO . async $
        (do
          threadDelay $ time * 1000000
          atomically $ writeTChan eventChan
              (LogEvent $
                "Connecting to FlightGear on " <>
                Text.pack fghost <> ":" <> Text.pack (show fgport))
          withFGFSConnection fghost fgport $ \fgconn -> forever $ do
            putMVar connVar fgconn
            atomically $ writeTChan eventChan (FGFSConnectEvent fgconn)
        )
        `catch`
        (\(e :: SomeException) -> atomically $ do
            writeTChan eventChan FGFSDisconnectEvent
            writeTChan eventChan (LogEvent $ "FGFS disconnected: " <> Text.pack (show e))
        )

      modify $ \s -> s
        { mcduFlightgearThread = Just fgfsThread
        }
    (Just _, _, _) -> do
      debugPrint . colorize blue $ "FlightGear already connected"
    _ -> do
      debugPrint . colorize blue $ "FlightGear not configured"

mcduDisconnectFlightgear :: MCDU ()
mcduDisconnectFlightgear = do
  gets mcduFlightgearThread >>= \case
    Nothing ->
      return ()
    Just thread -> do
      liftIO $ cancel thread
      modify $ \s -> s
        { mcduFlightgearConnection = Nothing
        , mcduFlightgearThread = Nothing
        }
      debugPrint . colorize magenta $
        "Disconnected from FlightGear"

mcduWithFGFS :: (FGFSConnection -> MCDU ()) -> MCDU ()
mcduWithFGFS action = do
  gets mcduFlightgearConnection >>= \case
    Nothing -> do
      scratchWarn "NO FGFS CONNECTION"
    Just conn ->
      action conn

debugPrint :: Colored Text -> MCDU ()
debugPrint str = do
  modify $ \s -> s
    { mcduDebugLog = str : mcduDebugLog s }
  redrawLog

handleKey :: MCDUKey -> MCDU ()
handleKey key =
  case key of
    MCDULSK lsk ->
      handleLSK lsk

    MCDUFunction PageUp ->
      prevPage
    MCDUFunction PageDown ->
      nextPage

    MCDUFunction DLK -> loadViewByID DLKMenuView
    MCDUFunction ATC -> loadViewByID ATCMenuView
    MCDUFunction FPL -> loadViewByID FPLView
    MCDUFunction RTE -> loadViewByID RTEView
    MCDUFunction NAV -> loadViewByID NAVView
    MCDUFunction Menu -> loadViewByID MainMenuView

    MCDUFunction DEL ->
      scratchClear
    MCDUFunction CLR ->
      scratchDel

    MCDUChar c ->
      if isAsciiLower c then
        scratchAppend (ord8 $ toUpper c)
      else if isAsciiUpper c ||
              isDigit c ||
              (c `elem` ['-', '.', '/', ' ']) then
        scratchAppend (ord8 c)
      else
        return ()

handleMCDUEvent :: MCDUEvent -> MCDU ()
handleMCDUEvent ev = do
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

    TickEvent -> do
      autoReload <- gets (mcduViewAutoReload . mcduView)
      when autoReload reloadView

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

    KeyEvent key -> do
      handleKey key

    RedrawEvent ->
      flushAll

    LogEvent cmd -> do
      debugPrint (colorize blue cmd)

    FGFSConnectEvent conn -> do
      debugPrint . colorize green $
        "Connected to FlightGear"
      modify $ \s -> s
        { mcduFlightgearConnection = Just conn
        }
      reloadView

    FGFSDisconnectEvent -> do
      debugPrint . colorize magenta $
        "Lost connection to FlightGear"
      modify $ \s -> s
        { mcduFlightgearConnection = Nothing
        , mcduFlightgearThread = Nothing
        }
      reloadView
      mcduConnectFlightgearAfter 10
