{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Hoppie.System
import Web.Hoppie.TUI.Input
import Web.Hoppie.TUI.MCDU
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Applicative
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
import Data.Maybe
import Data.Word
import qualified Data.Yaml as YAML
import Options.Applicative
import System.Environment
import System.FilePath
import Text.Casing
import Text.Read (readMaybe)

data ProgramOptions =
  ProgramOptions
    { poLogon :: Maybe String
    , poCallsign :: Maybe String
    , poAircraftType :: Maybe String
    , poFastPollingInterval :: Maybe Int
    , poSlowPollingInterval :: Maybe Int
    , poHoppieUrl :: Maybe String
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
    }

instance Semigroup ProgramOptions where
  ProgramOptions l1 c1 ty1 fp1 sp1 url1 <>
    ProgramOptions l2 c2 ty2 fp2 sp2 url2 =
      ProgramOptions
        (l1 <|> l2)
        (c1 <|> c2)
        (ty1 <|> ty2)
        (fp1 <|> fp2)
        (sp1 <|> sp2)
        (url1 <|> url2)

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
  return emptyProgramOptions
    { poLogon = logon
    , poCallsign = callsign
    , poHoppieUrl = url
    , poSlowPollingInterval = (slowPollingInterval <|> pollingInterval) >>= readMaybe
    , poFastPollingInterval = (fastPollingInterval <|> pollingInterval) >>= readMaybe
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
  let actype = fmap BS8.pack $ poAircraftType po
  runInput inputChan
    `race_`
    runInputPusher inputChan eventChan
    `race_`
    runSystem
      (BS8.pack callsign)
      config
      (handleUplink eventChan)
      (handleDownlink eventChan)
      (handleNetworkStatus eventChan)
      (handleCurrentDataAuthority eventChan)
      (hoppieMain actype eventChan)

runInputPusher :: TChan Word8 -> TChan MCDUEvent -> IO ()
runInputPusher inputChan eventChan = forever $ do
  readCommand inputChan >>= atomically . writeTChan eventChan . InputCommandEvent

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

hoppieMain :: Maybe ByteString -> TChan MCDUEvent -> (TypedMessage -> Hoppie ()) -> Hoppie ()
hoppieMain actypeMay eventChan rawSend = do
  runMCDU rawSend $ do
    modify $ \s -> s
      { mcduAircraftType = actypeMay }
    mcduMain eventChan

runColoredBSTests :: IO ()
runColoredBSTests = do
  let testStr = ColoredBS
        [ ColoredBSFragment 1 "HELLO WORLD "
        , ColoredBSFragment 2 "\nHOW ARE"
        , ColoredBSFragment 3 " YOU"
        , ColoredBSFragment 4 " THIS IS A FAIRLY LONG STRING WITH "
        , ColoredBSFragment 5 "PLENTY OF WORDS "
        ]
  print (coloredToBS testStr)
  print (coloredToBS $ coloredTake 10 testStr)
  print (coloredToBS $ coloredDrop 10 testStr)
  print (coloredFindIndex isSpace8 testStr)
  print (map coloredToBS $ coloredWordSplit testStr)
  print (map coloredToBS $ coloredLineSplit testStr)
  print (map coloredToBS $ lineWrap 20 testStr)
  print (lineWrap 20 $ coloredToBS testStr)
