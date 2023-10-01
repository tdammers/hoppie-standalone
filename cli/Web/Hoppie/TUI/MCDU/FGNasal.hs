{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Web.Hoppie.TUI.MCDU.FGNasal
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.NasalValue

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Control.Exception

withFGNasal_ :: (FGFSConnection -> MCDU ()) -> MCDU ()
withFGNasal_ = withFGNasalDef ()

withFGNasalBool :: (FGFSConnection -> MCDU Bool) -> MCDU Bool
withFGNasalBool = withFGNasalDef False

withFGNasal :: Monoid a => (FGFSConnection -> MCDU a) -> MCDU a
withFGNasal = withFGNasalDef mempty

withFGNasalDef :: forall a. a -> (FGFSConnection -> MCDU a) -> MCDU a
withFGNasalDef defval action = do
  connMay <- gets mcduFlightgearConnection
  case connMay of
    Nothing ->
      handleError "NO CONNECTION" Nothing
    Just conn -> do
      action conn `mcduCatches` handlers
  where

    handleError :: ByteString -> Maybe String -> MCDU a
    handleError scratchTxt logTxt = do
      forM_ logTxt $ debugPrint . colorize red . Text.pack
      scratchWarn scratchTxt
      return defval

    handlers :: [MCDUHandler a]
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


fgCallNasal :: forall a r. (ToNasal a, FromNasal r, Monoid r) => Text -> a -> MCDU r
fgCallNasal = fgCallNasalDef mempty

fgCallNasalBool :: forall a. (ToNasal a) => Text -> a -> MCDU Bool
fgCallNasalBool = fgCallNasalDef False

fgCallNasalDef :: forall a r. (ToNasal a, FromNasal r) => r -> Text -> a -> MCDU r
fgCallNasalDef defval func args =
  withFGNasalDef defval $ \conn -> do
    callNasalFunc conn func args

fgRunNasal :: forall r. (FromNasal r, Monoid r) => Text -> MCDU r
fgRunNasal = fgRunNasalDef mempty

fgRunNasalBool :: Text -> MCDU Bool
fgRunNasalBool = fgRunNasalDef False

fgRunNasalDef :: forall a. FromNasal a => a -> Text -> MCDU a
fgRunNasalDef defval script = do
  withFGNasalDef defval $ \conn -> do
    runNasal conn script

