{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Web.Hoppie.TUI.MCDU.Operations
( module Web.Hoppie.TUI.MCDU.Operations
, module Web.Hoppie.TUI.MCDU.Monad
, module Web.Hoppie.TUI.MCDU.Keys
)
where

import qualified Web.Hoppie.CPDLC.Message as CPDLC
import qualified Web.Hoppie.CPDLC.MessageTypes as CPDLC
import Web.Hoppie.System
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.FMS
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.HttpServer
import Web.Hoppie.TUI.MCDU.Keys
import Web.Hoppie.TUI.MCDU.Monad
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
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Exception

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

mcduGetCallsign :: MCDU ByteString
mcduGetCallsign = do
  fgsync <- gets mcduFlightgearSyncCallsign
  fgAvail <- gets (isJust . mcduFlightgearConnection)
  when (fgsync && fgAvail) $ do
    fgCallsign <- getFGCallsign
    mapM_ (lift . setCallsign) fgCallsign
  lift getCallsign

mcduSetCallsign :: ByteString -> MCDU ()
mcduSetCallsign cs = do
  lift $ setCallsign cs
  fgsync <- gets mcduFlightgearSyncCallsign
  fgAvail <- gets (isJust . mcduFlightgearConnection)
  when (fgsync && fgAvail) $ do
    setFGCallsign cs
  persistData

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

clearTelexBody :: MCDU ()
clearTelexBody = do
  modify $ \s -> s
    { mcduTelexBody = Nothing }
  persistData

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
    callsign <- lift $ mcduGetCallsign
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
setReferenceAirport = do
  scratchInteract
    (\val -> True <$ modify (\s -> s { mcduReferenceAirport = val }))
    (gets mcduReferenceAirport)
  persistData

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

loadViewAtPage :: Int -> MCDUView -> MCDU ()
loadViewAtPage page view = do
  join $ gets (mcduViewOnUnload . mcduView)
  modify $ \s -> s
    { mcduView = view { mcduViewPage = page } }
  reloadView

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
  fgConnect <- gets mcduFlightgearConnect
  when fgConnect $ do
    eventChan <- gets $ fromMaybe (error "Event chan not set up") . mcduEventChan
    fgthread <- gets mcduFlightgearThread
    fghostMay <- gets mcduFlightgearHostname
    fgportMay <- gets mcduFlightgearPort
    -- let logger = atomically . writeTChan eventChan . LogEvent . Text.pack
    let logger = const $ return ()
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
            withFGFSConnection (BS8.pack fghost) fgport logger $ \fgconn -> forever $ do
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
    MCDUFunction PROG -> loadViewByID PROGView
    MCDUFunction INIT -> loadViewByID INITView
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

    PersistEvent -> do
      persistData

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
      syncCS <- gets mcduFlightgearSyncCallsign
      when syncCS $ do
        cs <- getFGCallsign
        forM_ cs $ lift . setCallsign
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
