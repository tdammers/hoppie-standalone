{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Hoppie.TUI.MCDU.Monad
( module Web.Hoppie.TUI.MCDU.Monad
, module Web.Hoppie.TUI.MCDU.Keys
)
where

import Web.Hoppie.System
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.Monad
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.MCDU.Keys
import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex
import Web.Hoppie.TUI.MCDU.Views.Enum

import qualified Data.Text as Text
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Text (Text)
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

data MassUnit
  = Kilograms
  | Pounds
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

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
    , mcduMassUnit :: MassUnit
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

    , mcduFlightgearConnect :: Bool
    , mcduFlightgearConnection :: Maybe FGFSConnection
    , mcduFlightgearThread :: Maybe (Async ())
    , mcduFlightgearHostname :: Maybe String
    , mcduFlightgearPort :: Maybe Int
    , mcduFlightgearSyncCallsign :: Bool
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
    , mcduMassUnit = Kilograms
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

    , mcduFlightgearConnect = False
    , mcduFlightgearConnection = Nothing
    , mcduFlightgearThread = Nothing
    , mcduFlightgearHostname = Nothing
    , mcduFlightgearPort = Nothing
    , mcduFlightgearSyncCallsign = False
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

type MCDU = StateT MCDUState Hoppie

runMCDU :: (TypedMessage -> Hoppie ()) -> MCDU a -> Hoppie a
runMCDU rawSend = flip evalStateT defMCDUState { mcduSendMessage = rawSend }

instance MonadFG MCDU where
  withFGNasalDef defval action = do
    connMay <- gets mcduFlightgearConnection
    case connMay of
      Nothing ->
        handleError "NO CONNECTION" Nothing
      Just conn -> do
        action conn `mcduCatches` handlers
    where

      -- handleError :: ByteString -> Maybe String -> MCDU a
      handleError scratchTxt logTxt = do
        forM_ logTxt $ debugPrint . colorize red . Text.pack
        scratchWarn scratchTxt
        return defval

      -- handlers :: [MCDUHandler a]
      handlers =
        [ MCDUHandler $ \case
            NasalUnexpected expected found -> do
              handleError "SERVER ERROR" . Just $
                "Nasal value error: expected " <> expected <> ", but found " <> found
            NasalMissingKey key -> do
              handleError "SERVER ERROR" . Just $
                "Nasal value error: map key " <> key <> "missing"
        , MCDUHandler $ \case
            NasalRuntimeError msg stackTrace -> do
              handleError "SERVER ERROR" . Just $
                  "Nasal runtime error:" <> msg <> "\n" <>
                  unlines
                    [ fromMaybe "?" fileMay <> ":" <> maybe "-" show lineMay
                    | (fileMay, lineMay) <- stackTrace
                    ]
        , MCDUHandler $ \case
            JSONDecodeError err -> do
              handleError "JSON ERROR" . Just $ "JSON decoder error: " <> err
        , MCDUHandler $ \case
            FGFSConnectionClosed ->
              handleError "CONNECTION CLOSED" . Just $ "FlightGear connection closed."
            FGFSEndOfStream ->
              handleError "NETWORK ERROR" . Just $ "Unexpected end of stream"
            FGFSSocketError err ->
              handleError "NETWORK ERROR" . Just $ show err
            FGFSDNSError hostname ->
              handleError "DNS ERROR" . Just $ "DNS lookup failure trying to resolve " ++ show hostname
        , MCDUHandler $ \(e :: SomeException) -> do
              handleError "ERROR" . Just $ "Error:\n" <> show e
        ]
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

modifyView :: (MCDUView -> MCDUView) -> MCDU ()
modifyView f =  do
  modify $ \s -> s { mcduView = f (mcduView s) }

modifyScratchpad :: (ScratchVal -> ScratchVal) -> MCDU ()
modifyScratchpad f =  do
  modify $ \s -> s { mcduScratchpad = f (mcduScratchpad s) }
  redrawScratch

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

draw :: (forall s. MCDUDraw s ()) -> MCDU ()
draw action = do
  modify $ \s -> s {
    mcduScreenBuffer = runMCDUDraw action (mcduScreenBuffer s)
  }

flushAll :: MCDU ()
flushAll = do
  unlessHeadless $ do
    liftIO $ do
      clearScreen
      moveTo 0 0
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

flushScratch :: MCDU ()
flushScratch = do
  unlessHeadless $ do
    buf <- gets mcduScreenBuffer
    liftIO $ do
      redrawMCDULine (screenH - 1) buf
      moveTo 0 (screenH + 6)
      hFlush stdout
  sendScreenHttp

unlessHeadless :: MCDU () -> MCDU ()
unlessHeadless action = do
  headless <- gets mcduHeadless
  unless headless action

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

debugPrint :: Colored Text -> MCDU ()
debugPrint str = do
  modify $ \s -> s
    { mcduDebugLog = str : mcduDebugLog s }
  headless <- gets mcduHeadless
  if headless then
    liftIO $ rawPrintColored (str <> "\n")
  else
    redrawLog

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

