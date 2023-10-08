{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Vatsim
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Network.HTTP.Simple
import System.Random
import Text.Casing
import Data.Maybe

data StatusResponseOf txt =
  StatusResponse
    { statusData :: StatusDataOf txt
    , statusUser :: [txt]
    , statusMetar :: [txt]
    }
    deriving (Show, Read, Functor)

data StatusDataOf txt =
  StatusData
    { statusDataV3 :: [txt]
    , statusDataTransceivers :: [txt]
    , statusDataServers :: [txt]
    , statusDataServersSweatbox :: [txt]
    , statusDataServersAll :: [txt]
    }
    deriving (Show, Read, Functor)

data Datafeed =
  Datafeed
    { datafeedGeneral :: DatafeedGeneral
    , datafeedAtis :: [DatafeedAtis]
    }
    deriving (Show, Read)

data DatafeedGeneral =
  DatafeedGeneral
    { datafeedGeneralVersion :: Int
    , datafeedGeneralUpdateTimestamp :: UTCTime
    , datafeedGeneralConnectedClients :: Int
    , datafeedGeneralUniqueUsers :: Int
    }
    deriving (Show, Read)

data DatafeedAtis =
  DatafeedAtis
    { datafeedAtisCid :: Int
    , datafeedAtisName :: Maybe Text
    , datafeedAtisCallsign :: Maybe Text
    , datafeedAtisFrequency :: Maybe Text
    , datafeedAtisFacility :: Int
    , datafeedAtisRating :: Int
    , datafeedAtisServer :: Text
    , datafeedAtisVisualRange :: Int
    , datafeedAtisAtisCode :: Maybe Text
    , datafeedAtisTextAtis :: Maybe [Text]
    , datafeedAtisLastUpdated :: UTCTime
    , datafeedAtisLogonTime :: UTCTime
    }
    deriving (Show, Read)

$(deriveJSON
    (JSON.defaultOptions { JSON.fieldLabelModifier = toQuietSnake . dropPrefix . dropPrefix . fromHumps })
    ''StatusDataOf
  )
$(deriveJSON
    (JSON.defaultOptions { JSON.fieldLabelModifier = toQuietSnake . dropPrefix . fromHumps })
    ''StatusResponseOf
  )
$(deriveJSON
    (JSON.defaultOptions { JSON.fieldLabelModifier = toQuietSnake . dropPrefix . dropPrefix . fromHumps })
    ''DatafeedGeneral
  )
$(deriveJSON
    (JSON.defaultOptions { JSON.fieldLabelModifier = toQuietSnake . dropPrefix . dropPrefix . fromHumps })
    ''DatafeedAtis
  )
$(deriveJSON
    (JSON.defaultOptions { JSON.fieldLabelModifier = toQuietSnake . dropPrefix . fromHumps })
    ''Datafeed
  )

type StatusResponse = StatusResponseOf String

loadStatus :: IO StatusResponse
loadStatus = do
  srT <- getResponseBody <$> httpJSON "https://status.vatsim.net/status.json"
  return $ Text.unpack <$> srT

loadDatafeed :: StatusResponse -> IO Datafeed
loadDatafeed sr = do
  lbIndex <- randomRIO (0, length (statusDataV3 . statusData $ sr) - 1)
  let url = (statusDataV3 . statusData $ sr) !! lbIndex
  rq <- parseRequest url
  getResponseBody <$> httpJSON rq

runDatafeedFetcher :: IO (Async (), MVar Datafeed)
runDatafeedFetcher = do
  feedVar <- newEmptyMVar
  a <- runDatafeedFetcherWith feedVar
  return (a, feedVar)

runDatafeedFetcherWith :: MVar Datafeed -> IO (Async ())
runDatafeedFetcherWith feedVar = do
  sr <- loadStatus
  async . forever $ do
    feedMay <- (Just <$> loadDatafeed sr) `catch`
                  (\(e :: SomeException) -> do
                    print e
                    return Nothing
                  )
    forM_ feedMay $ \feed -> do
      _ <- tryTakeMVar feedVar
      putMVar feedVar feed

    -- wait 15 seconds
    threadDelay $ 15 * 1000000

getCurrentAtis :: MVar Datafeed -> Text -> IO (Maybe [Text])
getCurrentAtis feedVar callsign = do
  feedMay <- tryReadMVar feedVar
  case feedMay of
    Nothing ->
      return Nothing
    Just feed -> do
      return $ getCurrentAtisFrom feed callsign

getCurrentAtisFrom :: Datafeed -> Text -> Maybe [Text]
getCurrentAtisFrom feed callsign =
  Just
    [ Text.unwords atisLines
    | atis <- datafeedAtis feed
    , atisCallsign <- maybeToList $ datafeedAtisCallsign atis
    , callsign `Text.isPrefixOf` atisCallsign
    , atisLines <- maybeToList $ datafeedAtisTextAtis atis
    ]
