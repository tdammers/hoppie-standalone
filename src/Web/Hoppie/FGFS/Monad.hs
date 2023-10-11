{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.FGFS.Monad where

import Web.Hoppie.FGFS.NasalValue
import Web.Hoppie.FGFS.Connection

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State
import Control.Exception

class MonadIO m => MonadFG m where
  withFGNasalDef :: forall a. a -> (FGFSConnection -> m a) -> m a

data WithContext =
  WithContext String SomeException
  deriving (Show)

instance Exception WithContext where

fgCallNasal :: forall a r m. (ToNasal a, FromNasal r, Monoid r, MonadIO m, MonadFG m) => Text -> a -> m r
fgCallNasal = fgCallNasalDef mempty

fgCallNasalBool :: forall a m. (ToNasal a, MonadIO m, MonadFG m) => Text -> a -> m Bool
fgCallNasalBool = fgCallNasalDef False

fgCallNasalDef :: forall a r m. (ToNasal a, FromNasal r, MonadIO m, MonadFG m) => r -> Text -> a -> m r
fgCallNasalDef defval func args =
  withFGNasalDef defval $ \conn -> do
    liftIO $ callNasalFunc conn func args `catch` (throw . WithContext (Text.unpack func))

fgRunNasal :: forall r m. (FromNasal r, Monoid r, MonadIO m, MonadFG m) => Text -> m r
fgRunNasal = fgRunNasalDef mempty

fgRunNasalBool :: (MonadIO m, MonadFG m) => Text -> m Bool
fgRunNasalBool = fgRunNasalDef False

fgRunNasalDef :: forall a m. (FromNasal a, MonadIO m, MonadFG m) => a -> Text -> m a
fgRunNasalDef defval script = do
  withFGNasalDef defval $ \conn -> do
    runNasal conn script script

withFGNasal_ :: (MonadFG m) => (FGFSConnection -> m ()) -> m ()
withFGNasal_ = withFGNasalDef ()

withFGNasalBool :: (MonadFG m) => (FGFSConnection -> m Bool) -> m Bool
withFGNasalBool = withFGNasalDef False

withFGNasal :: (MonadFG m, Monoid a) => (FGFSConnection -> m a) -> m a
withFGNasal = withFGNasalDef mempty
