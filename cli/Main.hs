{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.System
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.MCDU
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String.QQ (s)
import Data.Text (Text)
import Data.Word
import qualified Data.Yaml as YAML
import Options.Applicative
import System.Environment
import System.FilePath
import Text.Casing
import Text.Printf
import Text.Read (readMaybe)
import System.IO.Error

data ProgramOptions =
  ProgramOptions
    { poLogon :: Maybe String
    , poCallsign :: Maybe String
    , poAircraftType :: Maybe String
    , poFastPollingInterval :: Maybe Int
    , poSlowPollingInterval :: Maybe Int
    , poHoppieUrl :: Maybe String
    , poShowLog :: Maybe Bool
    , poHttpServerHostname :: Maybe String
    , poHttpServerPort :: Maybe Int
    , poHeadless :: Maybe Bool
    , poFlightgearHostname :: Maybe String
    , poFlightgearPort :: Maybe Int
    , poFlightgearSyncCallsign :: Maybe Bool
    , poSessionFilename :: Maybe FilePath
    }
    deriving (Show)

emptyProgramOptions :: ProgramOptions
emptyProgramOptions =
  ProgramOptions
    { poLogon = Nothing
    , poCallsign = Nothing
    , poAircraftType = Nothing
    , poFastPollingInterval = Nothing
    , poSlowPollingInterval = Nothing
    , poHoppieUrl = Nothing
    , poShowLog = Nothing
    , poHttpServerHostname = Nothing
    , poHttpServerPort = Nothing
    , poHeadless = Nothing
    , poFlightgearHostname = Nothing
    , poFlightgearPort = Nothing
    , poFlightgearSyncCallsign = Nothing
    , poSessionFilename = Nothing
    }

defaultProgramOptions :: ProgramOptions
defaultProgramOptions =
  ProgramOptions
    { poLogon = Nothing
    , poCallsign = Just "TEST"
    , poAircraftType = Nothing
    , poFastPollingInterval = Just 20
    , poSlowPollingInterval = Just 60
    , poHoppieUrl = Just "http://www.hoppie.nl/acars/system/connect.html"
    , poShowLog = Just False
    , poHttpServerHostname = Nothing
    , poHttpServerPort = Nothing
    , poHeadless = Just False
    , poFlightgearHostname = Nothing
    , poFlightgearPort = Nothing
    , poFlightgearSyncCallsign = Nothing
    , poSessionFilename = Nothing
    }

instance Semigroup ProgramOptions where
  ProgramOptions l1 c1 ty1 fp1 sp1 url1 sl1 httph1 http1 hl1 fgh1 fgp1 fgsc1 sfn1 <>
    ProgramOptions l2 c2 ty2 fp2 sp2 url2 sl2 httph2 http2 hl2 fgh2 fgp2 fgsc2 sfn2 =
      ProgramOptions
        (l1 <|> l2)
        (c1 <|> c2)
        (ty1 <|> ty2)
        (fp1 <|> fp2)
        (sp1 <|> sp2)
        (url1 <|> url2)
        (sl1 <|> sl2)
        (httph1 <|> httph2)
        (http1 <|> http2)
        (hl1 <|> hl2)
        (fgh1 <|> fgh2)
        (fgp1 <|> fgp2)
        (fgsc1 <|> fgsc2)
        (sfn1 <|> sfn2)

$(deriveJSON
    JSON.defaultOptions
      { JSON.fieldLabelModifier = toKebab . dropPrefix . fromHumps
      , JSON.omitNothingFields = True
      }
    ''ProgramOptions
 )

optionsP :: Parser ProgramOptions
optionsP = ProgramOptions
  <$> option (Just <$> str)
        (  long "logon"
        <> short 'l'
        <> metavar "LOGON"
        <> help "Hoppie logon code"
        <> value Nothing
        )
  <*> option (Just <$> str)
        (  long "callsign"
        <> short 'c'
        <> metavar "CALLSIGN"
        <> help "Your callsign"
        <> value Nothing
        )
  <*> option (Just <$> str)
        (  long "aircraft-type"
        <> short 't'
        <> metavar "ICAO"
        <> help "ICAO aircraft type code for your aircraft (A32N, B738, ...)"
        <> value Nothing
        )
  <*> option (Just <$> auto)
        (  long "fast-polling"
        <> short 'P'
        <> metavar "INTERVAL"
        <> help "Fast polling interval, in seconds"
        <> value Nothing
        )
  <*> option (Just <$> auto)
        (  long "slow-polling"
        <> short 'p'
        <> metavar "INTERVAL"
        <> help "Slow polling interval, in seconds"
        <> value Nothing
        )
  <*> option (Just <$> str)
        (  long "hoppie-url"
        <> long "url"
        <> short 'u'
        <> metavar "URL"
        <> help "Hoppie ACARS connect URL"
        <> value Nothing
        )
  <*> flag
        Nothing
        (Just True)
        (  long "show-log"
        <> long "show-debug-log"
        <> help "Show debug log on screen"
        )
  <*> option (Just <$> str)
        (  long "http-server-hostname"
        <> metavar "HOSTNAME"
        <> help "Expose MCDU as an HTTP app on this hostname"
        <> value Nothing
        )
  <*> option (Just <$> auto)
        (  long "http-server-port"
        <> metavar "PORT"
        <> help "Expose MCDU as an HTTP app on this port"
        <> value Nothing
        )
  <*> flag
        Nothing
        (Just True)
        (  long "headless"
        <> short 'h'
        <> help "Headless mode (no terminal UI, only HTTP)"
        )
  <*> option (Just <$> str)
        (  long "flightgear-hostname"
        <> long "fgfs-hostname"
        <> metavar "HOSTNAME"
        <> help "Where to find a FlightGear telnet server"
        <> value Nothing
        )
  <*> option (Just <$> auto)
        (  long "flightgear-port"
        <> long "fgfs-port"
        <> metavar "PORT"
        <> help "Where to find a FlightGear telnet server"
        <> value Nothing
        )
  <*> flag
        Nothing
        (Just True)
        (  long "flightgear-sync-callsign"
        <> long "fgfs-sync-callsign"
        <> help "Sync callsign with FlightGear (requires fgfs-hostname and fgfs-port)"
        )
  <*> option (Just <$> str)
        (  long "session-file"
        <> long "session-filename"
        <> metavar "FILE"
        <> help "Where to store session"
        <> value Nothing
        )

optionsFromArgs :: IO ProgramOptions
optionsFromArgs =
  execParser p
  where
    p = info
          (optionsP <**> helper)
          (  fullDesc
          <> header "hoppie-mcdu - an MCDU for Hoppie ACARS/CPDLC in the terminal"
          )

optionsFromConfigFile :: IO ProgramOptions
optionsFromConfigFile =
  fmap (fromMaybe emptyProgramOptions) $
    runMaybeT $ do
      home <- MaybeT $ lookupEnv "HOME"
      let configFilePath = home </> ".config" </> "hoppie-mcdu" </> "config.yaml"
      MaybeT $ liftIO
        (loadFile configFilePath
          `catch` (\(e :: SomeException) -> print e >> return Nothing)
        )
  where
    loadFile :: FilePath -> IO (Maybe ProgramOptions)
    loadFile configFilePath = do
      yamlResult <- YAML.decodeFileEither configFilePath
      case yamlResult of
        Left err -> do
          putStrLn $ YAML.prettyPrintParseException err
          return Nothing
        Right config ->
          return (Just config)

optionsFromEnv :: IO ProgramOptions
optionsFromEnv = do
  logon <- lookupEnv "HOPPIE_LOGON"
  callsign <- lookupEnv "HOPPIE_CALLSIGN"
  url <- lookupEnv "HOPPIE_URL"
  pollingInterval <- lookupEnv "HOPPIE_POLLING_INTERVAL"
  slowPollingInterval <- lookupEnv "HOPPIE_SLOW_POLLING_INTERVAL"
  fastPollingInterval <- lookupEnv "HOPPIE_FAST_POLLING_INTERVAL"
  httpPort <- lookupEnv "HOPPIE_HTTP_PORT"
  httpHostname <- lookupEnv "HOPPIE_HTTP_HOST"
  return emptyProgramOptions
    { poLogon = logon
    , poCallsign = callsign
    , poHoppieUrl = url
    , poSlowPollingInterval = (slowPollingInterval <|> pollingInterval) >>= readMaybe
    , poFastPollingInterval = (fastPollingInterval <|> pollingInterval) >>= readMaybe
    , poHttpServerPort = httpPort >>= readMaybe
    , poHttpServerHostname = httpHostname
    }

optionsToConfig :: ProgramOptions -> Either String Config
optionsToConfig po = runExcept $ do
  logon <- maybe (throwError "No logon configured") return $ poLogon po
  url <- maybe (throwError "No URL configured") return $ poHoppieUrl po
  fastPollingInterval <- maybe (throwError "No fast polling configured") return $ poFastPollingInterval po
  slowPollingInterval <- maybe (throwError "No slow polling configured") return $ poSlowPollingInterval po
  return $
    Config
      (BS8.pack logon)
      url
      slowPollingInterval
      fastPollingInterval

hoppieMainOptionsFromProgramOptions :: ProgramOptions -> HoppieMainOptions
hoppieMainOptionsFromProgramOptions po =
  let headless = fromMaybe False $ poHeadless po
      actype = fmap BS8.pack $ poAircraftType po
      showLog = fromMaybe False $ poShowLog po
      httpPort = poHttpServerPort po
      httpHostname = poHttpServerHostname po
      fgPort = poFlightgearPort po
      fgHostname = poFlightgearHostname po
      fgSyncCallsign = poFlightgearSyncCallsign po
      pfile = poSessionFilename po
  in
    defHoppieMainOptions
      { hoppieMainShowLog = showLog
      , hoppieMainHeadless = headless
      , hoppieMainHttpHostname = httpHostname
      , hoppieMainHttpPort = httpPort
      , hoppieMainFlightgearHostname = fgHostname
      , hoppieMainFlightgearPort = fgPort
      , hoppieMainFlightgearSyncCallsign = fgSyncCallsign
      , hoppieMainAircraftType = actype
      , hoppieMainPersistenceFile = pfile
      }

main :: IO ()
main = do
  inputChan <- newTChanIO
  eventChan <- newTChanIO
  poFromEnv <- optionsFromEnv
  poFromConfigFile <- optionsFromConfigFile
  poFromArgs <- optionsFromArgs
  let po = poFromArgs <> poFromEnv <> poFromConfigFile <> defaultProgramOptions
  config <- either error return $ optionsToConfig po
  callsign <- maybe (error "No callsign configured") return $ poCallsign po
  let hmo = hoppieMainOptionsFromProgramOptions po
  let hooks = HoppieHooks
                { onUplink = handleUplink eventChan
                , onDownlink = handleDownlink eventChan
                , onNetworkStatus = handleNetworkStatus eventChan
                , onCpdlcLogon = handleCurrentDataAuthority eventChan
                , onDataUpdated = handleDataUpdated (hoppieMainPersistenceFile hmo)
                }
  runInput inputChan
    `race_`
    runInputPusher inputChan eventChan
    `race_`
    runTickTimer eventChan
    `race_`
    runSystem
      (BS8.pack callsign)
      config
      hooks
      (hoppieMain hmo eventChan)

runInputPusher :: TChan Word8 -> TChan MCDUEvent -> IO ()
runInputPusher inputChan eventChan = do
  kcl <- loadKeyCodes
  forever $ do
    inputCommand <- readCommand kcl inputChan
    forM_ (mapInputCommand inputCommand) $ \ev ->
      atomically . writeTChan eventChan $ ev

mapInputCommand :: InputCommand -> Maybe MCDUEvent
mapInputCommand (InputChar c) = Just $ KeyEvent (MCDUChar c)
mapInputCommand InputF1 = Just $ KeyEvent (MCDULSK $ LSKL 0)
mapInputCommand InputF2 = Just $ KeyEvent (MCDULSK $ LSKL 1)
mapInputCommand InputF3 = Just $ KeyEvent (MCDULSK $ LSKL 2)
mapInputCommand InputF4 = Just $ KeyEvent (MCDULSK $ LSKL 3)
mapInputCommand InputF5 = Just $ KeyEvent (MCDULSK $ LSKL 4)
mapInputCommand InputF6 = Just $ KeyEvent (MCDULSK $ LSKL 5)
mapInputCommand InputF7 = Just $ KeyEvent (MCDULSK $ LSKR 0)
mapInputCommand InputF8 = Just $ KeyEvent (MCDULSK $ LSKR 1)
mapInputCommand InputF9 = Just $ KeyEvent (MCDULSK $ LSKR 2)
mapInputCommand InputF10 = Just $ KeyEvent (MCDULSK $ LSKR 3)
mapInputCommand InputF11 = Just $ KeyEvent (MCDULSK $ LSKR 4)
mapInputCommand InputF12 = Just $ KeyEvent (MCDULSK $ LSKR 5)
mapInputCommand InputEscape = Just $ KeyEvent (MCDUFunction Menu)
mapInputCommand InputDel = Just $ KeyEvent (MCDUFunction DEL)
mapInputCommand InputBackspace = Just $ KeyEvent (MCDUFunction CLR)
mapInputCommand InputF13 = Just $ KeyEvent (MCDUFunction DLK)
mapInputCommand InputF14 = Just $ KeyEvent (MCDUFunction ATC)
mapInputCommand InputF15 = Just $ KeyEvent (MCDUFunction FPL)
mapInputCommand InputF16 = Just $ KeyEvent (MCDUFunction RTE)
mapInputCommand InputPgUp = Just $ KeyEvent (MCDUFunction PageUp)
mapInputCommand InputPgDn = Just $ KeyEvent (MCDUFunction PageDown)
mapInputCommand _ = Nothing

runTickTimer :: TChan MCDUEvent -> IO ()
runTickTimer eventChan = liftIO . forever $ do
  atomically . writeTChan eventChan $ TickEvent
  threadDelay 10000000

handleUplink :: TChan MCDUEvent -> WithMeta UplinkStatus TypedMessage -> Hoppie ()
handleUplink eventChan = do
  liftIO . atomically . writeTChan eventChan . UplinkEvent

handleDownlink :: TChan MCDUEvent -> WithMeta DownlinkStatus TypedMessage -> Hoppie ()
handleDownlink eventChan = do
  liftIO . atomically . writeTChan eventChan . DownlinkEvent

handleNetworkStatus :: TChan MCDUEvent -> NetworkStatus -> Hoppie ()
handleNetworkStatus eventChan = do
  liftIO . atomically . writeTChan eventChan . NetworkStatusEvent

handleCurrentDataAuthority :: TChan MCDUEvent -> Maybe ByteString -> Hoppie ()
handleCurrentDataAuthority eventChan = do
  liftIO . atomically . writeTChan eventChan . CurrentDataAuthorityEvent

handleDataUpdated :: Maybe FilePath -> Hoppie ()
handleDataUpdated Nothing = return ()
handleDataUpdated (Just ppath) = do
  pdata <- getPersistentData
  liftIO $ JSON.encodeFile ppath pdata

data HoppieMainOptions =
  HoppieMainOptions
    { hoppieMainShowLog :: Bool
    , hoppieMainHeadless :: Bool
    , hoppieMainHttpHostname :: Maybe String
    , hoppieMainHttpPort :: Maybe Int
    , hoppieMainFlightgearHostname :: Maybe String
    , hoppieMainFlightgearPort :: Maybe Int
    , hoppieMainFlightgearSyncCallsign :: Maybe Bool
    , hoppieMainAircraftType :: Maybe ByteString
    , hoppieMainPersistenceFile :: Maybe FilePath
    }
    deriving (Show)

defHoppieMainOptions :: HoppieMainOptions
defHoppieMainOptions =
  HoppieMainOptions
    { hoppieMainShowLog = True
    , hoppieMainHeadless = False
    , hoppieMainHttpHostname = Nothing
    , hoppieMainHttpPort = Nothing
    , hoppieMainFlightgearHostname = Nothing
    , hoppieMainFlightgearPort = Nothing
    , hoppieMainFlightgearSyncCallsign = Nothing
    , hoppieMainAircraftType = Nothing
    , hoppieMainPersistenceFile = Nothing
    }

applyHoppieMainOptions :: HoppieMainOptions -> MCDUState -> MCDUState
applyHoppieMainOptions hmo m =
  m
    { mcduAircraftType = hoppieMainAircraftType hmo <|> mcduAircraftType m
    , mcduShowLog = hoppieMainShowLog hmo
    , mcduHttpHostname = hoppieMainHttpHostname hmo
    , mcduHttpPort = hoppieMainHttpPort hmo
    , mcduFlightgearHostname = hoppieMainFlightgearHostname hmo
    , mcduFlightgearPort = hoppieMainFlightgearPort hmo
    , mcduFlightgearSyncCallsign = fromMaybe False $ hoppieMainFlightgearSyncCallsign hmo
    , mcduFlightgearConnect =
        isJust (hoppieMainFlightgearHostname hmo) &&
        isJust (hoppieMainFlightgearPort hmo)
    , mcduHeadless = hoppieMainHeadless hmo
    }


hoppieMain :: HoppieMainOptions
           -> TChan MCDUEvent
           -> (TypedMessage -> Hoppie ()) -> Hoppie ()
hoppieMain hmo eventChan rawSend = do
  forM_ (hoppieMainPersistenceFile hmo) $ \ppath -> do
    sessionMay <- liftIO $ do
        (JSON.eitherDecodeFileStrict' ppath >>= \case
            Left e -> do
              atomically $ writeTChan eventChan $ LogEvent ("Failed to load session: " <> Text.pack e)
              return Nothing
            Right session -> do
              atomically $ writeTChan eventChan $ LogEvent "Session loaded"
              return (Just session)
          ) `catch` (\e -> do
            if isDoesNotExistError e then do
              atomically $ writeTChan eventChan $ LogEvent ("Session file does not exist: " <> Text.pack ppath)
              return Nothing
            else do
              atomically $ writeTChan eventChan $ LogEvent ("Error reading session file " <> Text.pack ppath <> ": " <> Text.pack (show e))
              return Nothing
          )
    forM_ sessionMay restorePersistentData
  runMCDU rawSend $ do
    modify (applyHoppieMainOptions hmo)
    mcduMain eventChan

runColoredTests :: IO ()
runColoredTests = do
  print $ lineWrap 20
    ("Hello, this is a long string that should be split into several lines." :: String)

runInputTest :: IO ()
runInputTest = do
  kcl <- loadKeyCodes

  let showTerminalChar c
        | c >= 32 && c < 127
        = printf "(%02x %u %c)" c c (chr8 c)
        | otherwise
        = printf "(%02x %u)" c c
  forM_ (sortOn snd $ Map.toAscList kcl) $ \(chars, cmd) -> do
    printf "%-10s = %s\n"
      (show cmd)
      (unwords $ map showTerminalChar chars)

  inputChan <- newTChanIO
  race_
    (runInput inputChan)
    (runLogger kcl inputChan)
  where
    runLogger kcl inputChan = forever $ do
      c <- readCommand kcl inputChan
      print c

runFGFSTest :: IO ()
runFGFSTest = do
  withFGFSConnection "localhost" 10000 putStrLn $ \conn -> do
    (flightplan :: NasalGhost "flightplan") <- runNasal conn "return flightplan();"
    print flightplan
    print (encodeNasal flightplan)
    runNasal_ conn $ "debug.dump(" <> encodeNasal flightplan <> "); return nil"
    sidMay <- runNasal conn
      [s| var fp = flightplan();
          if (fp.sid == nil)
            return nil;
          else
            return fp.sid.id;
        |]
    starMay <- runNasal conn
      [s| var fp = flightplan();
          if (fp.star == nil)
            return nil;
          else
            return fp.star.id;
        |]
    waypoints <- runNasal conn
      [s| var fp = flightplan();
          var result = [];
          for (var i = 0; i < fp.getPlanSize(); i += 1) {
            var wp = fp.getWP(i);
            var parent_id = nil;
            if (wp.wp_parent != nil)
              parent_id = wp.wp_parent.id;
            append(result, [wp.wp_name, wp.wp_role, parent_id]);
          }
          return result;
        |]
    currentWP <- runNasal conn
      [s| var fp = flightplan();
          if (fp == nil)
            return nil;
          else
            return fp.current;
        |]
    printf "SID: %s\n" (fromMaybe "n/a" sidMay :: Text)
    printf "STAR: %s\n" (fromMaybe "n/a" starMay :: Text)
    putStrLn "WAYPOINTS:"
    zipWithM_
        (\i (wp_name, wp_role, wp_parent_id) -> do
          printf "%s %3i %-10s %-8s %s\n"
            (if Just i == currentWP then "*" else " " :: Text)
            i
            wp_name
            (fromMaybe "-" wp_role)
            (fromMaybe "-" wp_parent_id)
        )
        ([0,1..] :: [Int]) (waypoints :: [(Text, Maybe Text, Maybe Text)])
    when (null waypoints) $ do
      putStrLn "--- NO WAYPOINTS ---"
    runNasal_ conn "debug.dump(refTable);"
