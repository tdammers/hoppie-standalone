{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Hoppie.FGFS.NasalValue
where

import Control.Applicative
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as JSON
import Data.Map (Map)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Printf
import Control.Exception
import Debug.Trace

data NasalDecodeError
  = NasalUnexpected String String
  deriving (Show)

instance Exception NasalDecodeError where

data NasalValue
  = NasalNil
  | NasalString !Text
  | NasalInt !Int
  | NasalFloat !Double
  | NasalVector !(Vector NasalValue)
  | NasalHash !(Map Text NasalValue)
  deriving (Show, Eq, Ord)

instance FromJSON NasalValue where
  parseJSON JSON.Null = pure NasalNil
  parseJSON (JSON.String txt) = pure $ NasalString txt
  parseJSON (JSON.Number num) =
    pure $ either NasalFloat NasalInt $ floatingOrInteger num
  parseJSON (JSON.Array xs) =
    NasalVector <$> Vector.mapM parseJSON xs
  parseJSON j@JSON.Object {} =
    NasalHash <$> parseJSON j
  parseJSON (JSON.Bool True) =
    pure $ NasalInt 1
  parseJSON (JSON.Bool False) =
    pure $ NasalInt 0

instance ToJSON NasalValue where
  toJSON NasalNil = JSON.Null
  toJSON (NasalString txt) = toJSON txt
  toJSON (NasalInt i) = toJSON i
  toJSON (NasalFloat f) = toJSON f
  toJSON (NasalVector v) = toJSON v
  toJSON (NasalHash m) = toJSON m

data NasalValueOrError
  = NasalValue NasalValue
  | NasalError NasalError
  deriving (Show, Eq)

data NasalError =
  NasalRuntimeError
    { nasalRuntimeErrorMessage :: String
    , nasalRuntimeErrorSourceFile :: Maybe FilePath
    , nasalRuntimeErrorSourceLine :: Maybe Int
    }
    deriving (Show, Eq)

instance Exception NasalError where

instance FromJSON NasalError where
  parseJSON j = do
    (err, sourceMay, lineMay) <- parseJSON j
    return $ NasalRuntimeError err sourceMay lineMay

instance FromJSON NasalValueOrError where
  parseJSON = JSON.withObject "NasalValueOrError" $ \obj -> do
    errMay <- fmap NasalError <$> obj .:? "error"
    case errMay of
      Just err ->
        return err
      Nothing -> do
        NasalValue <$> (parseJSON =<< obj .: "value")

class FromNasal a where
  fromNasal :: NasalValue -> Either NasalDecodeError a

instance FromNasal NasalValue where
  fromNasal = pure

instance FromNasal () where
  fromNasal NasalNil = pure ()
  fromNasal n = Left $ NasalUnexpected "nil" (show n)

instance FromNasal Int where
  fromNasal (NasalInt i) = pure i
  fromNasal n = Left $ NasalUnexpected "integer" (show n)

instance FromNasal Double where
  fromNasal (NasalInt i) = pure $ fromIntegral i
  fromNasal (NasalFloat f) = pure f
  fromNasal n = Left $ NasalUnexpected "float" (show n)

instance FromNasal Text where
  fromNasal (NasalString txt) = pure txt
  fromNasal (NasalInt i) = pure . Text.pack . show $ i
  fromNasal (NasalFloat f) = pure . Text.pack . printf "%1.6f" $ f
  fromNasal n = Left $ NasalUnexpected "string" (show n)

instance FromNasal a => FromNasal (Maybe a) where
  fromNasal NasalNil = pure Nothing
  fromNasal n = Just <$> fromNasal n

instance FromNasal a => FromNasal [a] where
  fromNasal (NasalVector v) = mapM fromNasal $ Vector.toList v
  fromNasal n = Left $ NasalUnexpected "vector" (show n)

instance FromNasal a => FromNasal (Vector a) where
  fromNasal (NasalVector v) = Vector.mapM fromNasal v
  fromNasal n = Left $ NasalUnexpected "vector" (show n)

instance FromNasal a => FromNasal (Map Text a) where
  fromNasal (NasalHash m) =
    mapM fromNasal m
  fromNasal n = Left $ NasalUnexpected "object" (show n)

instance (FromNasal a, FromNasal b) => FromNasal (a, b) where
  fromNasal (NasalVector [a, b]) =
    (,)
      <$> fromNasal a
      <*> fromNasal b
  fromNasal n = Left $ NasalUnexpected "vector[2]" (show n)

instance (FromNasal a, FromNasal b, FromNasal c) => FromNasal (a, b, c) where
  fromNasal (NasalVector [a, b, c]) =
    (,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
  fromNasal n = Left $ NasalUnexpected "vector[3]" (show n)

instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d) => FromNasal (a, b, c, d) where
  fromNasal (NasalVector [a, b, c, d]) =
    (,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
  fromNasal n = Left $ NasalUnexpected "vector[4]" (show n)

instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d, FromNasal e) => FromNasal (a, b, c, d, e) where
  fromNasal (NasalVector [a, b, c, d, e]) =
    (,,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
      <*> fromNasal e
  fromNasal n = Left $ NasalUnexpected "vector[5]" (show n)

instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d, FromNasal e, FromNasal f) => FromNasal (a, b, c, d, e, f) where
  fromNasal (NasalVector [a, b, c, d, e, f]) =
    (,,,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
      <*> fromNasal e
      <*> fromNasal f
  fromNasal n = Left $ NasalUnexpected "vector[6]" (show n)

instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d, FromNasal e, FromNasal f, FromNasal g) => FromNasal (a, b, c, d, e, f, g) where
  fromNasal (NasalVector [a, b, c, d, e, f, g]) =
    (,,,,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
      <*> fromNasal e
      <*> fromNasal f
      <*> fromNasal g
  fromNasal n = Left $ NasalUnexpected "vector[7]" (show n)
