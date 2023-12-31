{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Web.Hoppie.FGFS.NasalValue
where

import Control.Applicative
import Control.Exception
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.:?), (.=) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import GHC.TypeLits
import Text.Printf

data NasalDecodeError
  = NasalUnexpected String String
  | NasalMissingKey String
  deriving (Show)

instance Exception NasalDecodeError where

data NasalGhost (ghosttype :: Symbol) =
  NasalGhost !Text
  deriving (Eq, Ord)

instance KnownSymbol ghosttype => Show (NasalGhost ghosttype) where
  show (NasalGhost ref) =
    "NasalGhost @" ++ show (symbolVal (Proxy @ghosttype)) ++ " " ++ show ref

data NasalValue
  = NasalNil
  | NasalString !Text
  | NasalInt !Int
  | NasalFloat !Double
  | NasalVector !(Vector NasalValue)
  | NasalHash !(Map Text NasalValue)
  | NasalGhostRef !Text !Text
  | NasalFunctionRef !Text
  deriving (Show, Eq, Ord)

encodeNasalValue :: NasalValue -> Text
encodeNasalValue NasalNil = "nil"
encodeNasalValue (NasalString str) = encodeNasalString str
encodeNasalValue (NasalInt i) = Text.pack $ show i
encodeNasalValue (NasalFloat f) = Text.pack $ printf "%1.12f" f
encodeNasalValue (NasalVector v) =
  "[" <> (Text.intercalate "," . Vector.toList . Vector.map encodeNasalValue $ v) <> "]"
encodeNasalValue (NasalHash m) =
  "{" <>
  (Text.intercalate "," [ Text.pack (show k) <> ": " <> encodeNasalValue v | (k, v) <- Map.toList m ]) <>
  "}"
encodeNasalValue (NasalGhostRef _ ident) =
  "globals.externalMCDU.deref({'__reftype__': 'ghost', '__refid__': " <> Text.pack (show ident) <> "})"
encodeNasalValue (NasalFunctionRef ident) =
  "globals.externalMCDU.deref({'__reftype__': 'func', '__refid__': " <> Text.pack (show ident) <> "})"

encodeNasal :: ToNasal a => a -> Text
encodeNasal = encodeNasalValue . toNasal

encodeNasalString :: Text -> Text
encodeNasalString txt = "\"" <> (mconcat . map encodeNasalStringChar . Text.unpack $ txt) <> "\""

encodeNasalStringChar :: Char -> Text
encodeNasalStringChar '"' = "\\\""
encodeNasalStringChar '\n' = "\\n"
encodeNasalStringChar '\r' = "\\r"
encodeNasalStringChar '\t' = "\\t"
encodeNasalStringChar '\0' = "\\0"
encodeNasalStringChar '\\' = "\\\\"
encodeNasalStringChar c = Text.singleton c

instance FromJSON NasalValue where
  parseJSON JSON.Null = pure NasalNil
  parseJSON (JSON.String txt) = pure $ NasalString txt
  parseJSON (JSON.Number num) =
    pure $ either NasalFloat NasalInt $ floatingOrInteger num
  parseJSON (JSON.Array xs) =
    NasalVector <$> Vector.mapM parseJSON xs
  parseJSON j@(JSON.Object obj) = do
    (refTypeMay :: Maybe Text) <- obj .:? "__reftype__"
    case refTypeMay of
      Just "ghost" -> do
        NasalGhostRef
          <$> obj .: "__ghosttype__"
          <*> obj .: "__refid__"
      Just "func" -> do
        NasalFunctionRef
          <$> obj .: "__refid__"
      _ -> NasalHash <$> parseJSON j
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
  toJSON (NasalGhostRef ty ident) =
    JSON.object
      [ "__ghosttype__" .= ty
      , "__ghostid__" .= ident
      ]
  toJSON (NasalFunctionRef ident) =
    JSON.object
      [ "__funcid__" .= ident
      ]

data NasalValueOrError
  = NasalValue NasalValue
  | NasalError NasalError
  deriving (Show, Eq)

data NasalError =
  NasalRuntimeError
    { nasalRuntimeErrorMessage :: String
    , nasalRuntimeErrorStackTrace :: [(Maybe String, Maybe Int)]
    }
    deriving (Show, Eq)

instance Exception NasalError where

instance FromJSON NasalError where
  parseJSON j@(JSON.Array v) = do
    case Vector.toList v of
      (x:xs) -> do
        err <- parseJSON x
        stackTrace <- parseStackTrace xs
        return $ NasalRuntimeError err stackTrace
      _ -> JSON.typeMismatch "non-empty Array" j
    where
      parseStackTrace [] =
        pure []
      parseStackTrace [_] =
        JSON.typeMismatch "uneven-sized Array" j
      parseStackTrace (x:y:xs) = do
        src <- parseJSON x
        l <- parseJSON y
        r <- parseStackTrace xs
        return $ (src, l):r
  parseJSON j = do
    JSON.typeMismatch "non-empty Array" j

instance FromJSON NasalValueOrError where
  parseJSON = JSON.withObject "NasalValueOrError" $ \obj -> do
    errMay <- fmap NasalError <$> obj .:? "e"
    case errMay of
      Just err ->
        return err
      Nothing -> do
        NasalValue <$> (parseJSON =<< obj .: "v")

class ToNasal a where
  toNasal :: a -> NasalValue

class FromNasal a where
  fromNasal :: NasalValue -> Either NasalDecodeError a

fromNasalField :: FromNasal a => Text -> NasalValue -> Either NasalDecodeError a
fromNasalField key (NasalHash m) =
  case Map.lookup key m of
    Nothing -> Left $ NasalMissingKey (Text.unpack key)
    Just n -> fromNasal n
fromNasalField _ n = Left $ NasalUnexpected "hash" (show n)

fromNasalFieldMaybe :: FromNasal a => Text -> NasalValue -> Either NasalDecodeError (Maybe a)
fromNasalFieldMaybe key (NasalHash m) =
  case Map.lookup key m of
    Nothing -> return Nothing
    Just n -> fromNasal n
fromNasalFieldMaybe _ n = Left $ NasalUnexpected "hash" (show n)

instance ToNasal NasalValue where
  toNasal = id

instance FromNasal NasalValue where
  fromNasal = pure

instance KnownSymbol tySym => ToNasal (NasalGhost tySym) where
  toNasal (NasalGhost ref) =
    NasalGhostRef (Text.pack $ symbolVal (Proxy @tySym)) ref

instance KnownSymbol tySym => FromNasal (NasalGhost tySym) where
  fromNasal (NasalGhostRef ty ref)
    | Text.unpack ty == symbolVal (Proxy @tySym)
    = pure $ NasalGhost ref
  fromNasal n = Left $ NasalUnexpected "ghost" (show n)

instance ToNasal () where
  toNasal _ = NasalNil

instance FromNasal () where
  fromNasal NasalNil = pure ()
  fromNasal n = Left $ NasalUnexpected "nil" (show n)

instance ToNasal Bool where
  toNasal True = NasalInt 1
  toNasal False = NasalInt 0
instance FromNasal Bool where
  fromNasal (NasalInt i) = pure (i /= 0)
  fromNasal n = Left $ NasalUnexpected "integer" (show n)

instance ToNasal Int where
  toNasal = NasalInt

instance FromNasal Int where
  fromNasal (NasalInt i) = pure i
  fromNasal n = Left $ NasalUnexpected "integer" (show n)

instance ToNasal Double where
  toNasal = NasalFloat

instance FromNasal Double where
  fromNasal (NasalInt i) = pure $ fromIntegral i
  fromNasal (NasalFloat f) = pure f
  fromNasal n = Left $ NasalUnexpected "float" (show n)

instance ToNasal Text where
  toNasal = NasalString
instance FromNasal Text where
  fromNasal (NasalString txt) = pure txt
  fromNasal (NasalInt i) = pure . Text.pack . show $ i
  fromNasal (NasalFloat f) = pure . Text.pack . printf "%1.6f" $ f
  fromNasal n = Left $ NasalUnexpected "string" (show n)

instance ToNasal ByteString where
  toNasal = NasalString . decodeUtf8
instance FromNasal ByteString where
  fromNasal (NasalString txt) = pure (encodeUtf8 txt)
  fromNasal (NasalInt i) = pure . BS8.pack . show $ i
  fromNasal (NasalFloat f) = pure . BS8.pack . printf "%1.6f" $ f
  fromNasal n = Left $ NasalUnexpected "string" (show n)

instance ToNasal a => ToNasal (Maybe a) where
  toNasal Nothing = NasalNil
  toNasal (Just x) = toNasal x
instance FromNasal a => FromNasal (Maybe a) where
  fromNasal NasalNil = pure Nothing
  fromNasal n = Just <$> fromNasal n

instance ToNasal a => ToNasal [a] where
  toNasal = NasalVector . Vector.fromList . map toNasal
instance FromNasal a => FromNasal [a] where
  fromNasal (NasalVector v) = mapM fromNasal $ Vector.toList v
  fromNasal n = Left $ NasalUnexpected "vector" (show n)

instance ToNasal a => ToNasal (Vector a) where
  toNasal = NasalVector . Vector.map toNasal
instance FromNasal a => FromNasal (Vector a) where
  fromNasal (NasalVector v) = Vector.mapM fromNasal v
  fromNasal n = Left $ NasalUnexpected "vector" (show n)

instance ToNasal a => ToNasal (Map Text a) where
  toNasal = NasalHash . fmap toNasal
instance FromNasal a => FromNasal (Map Text a) where
  fromNasal (NasalHash m) =
    mapM fromNasal m
  fromNasal n = Left $ NasalUnexpected "object" (show n)

instance (ToNasal a, ToNasal b) => ToNasal (a, b) where
  toNasal (a, b) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b]
instance (FromNasal a, FromNasal b) => FromNasal (a, b) where
  fromNasal (NasalVector [a, b]) =
    (,)
      <$> fromNasal a
      <*> fromNasal b
  fromNasal n = Left $ NasalUnexpected "vector[2]" (show n)

instance (ToNasal a, ToNasal b, ToNasal c) => ToNasal (a, b, c) where
  toNasal (a, b, c) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b, toNasal c]
instance (FromNasal a, FromNasal b, FromNasal c) => FromNasal (a, b, c) where
  fromNasal (NasalVector [a, b, c]) =
    (,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
  fromNasal n = Left $ NasalUnexpected "vector[3]" (show n)

instance (ToNasal a, ToNasal b, ToNasal c, ToNasal d) => ToNasal (a, b, c, d) where
  toNasal (a, b, c, d) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b, toNasal c, toNasal d]
instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d) => FromNasal (a, b, c, d) where
  fromNasal (NasalVector [a, b, c, d]) =
    (,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
  fromNasal n = Left $ NasalUnexpected "vector[4]" (show n)

instance (ToNasal a, ToNasal b, ToNasal c, ToNasal d, ToNasal e) => ToNasal (a, b, c, d, e) where
  toNasal (a, b, c, d, e) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b, toNasal c, toNasal d, toNasal e]
instance (FromNasal a, FromNasal b, FromNasal c, FromNasal d, FromNasal e) => FromNasal (a, b, c, d, e) where
  fromNasal (NasalVector [a, b, c, d, e]) =
    (,,,,)
      <$> fromNasal a
      <*> fromNasal b
      <*> fromNasal c
      <*> fromNasal d
      <*> fromNasal e
  fromNasal n = Left $ NasalUnexpected "vector[5]" (show n)

instance (ToNasal a, ToNasal b, ToNasal c, ToNasal d, ToNasal e, ToNasal f) => ToNasal (a, b, c, d, e, f) where
  toNasal (a, b, c, d, e, f) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b, toNasal c, toNasal d, toNasal e, toNasal f]
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

instance (ToNasal a, ToNasal b, ToNasal c, ToNasal d, ToNasal e, ToNasal f, ToNasal g) => ToNasal (a, b, c, d, e, f, g) where
  toNasal (a, b, c, d, e, f, g) =
    NasalVector $ Vector.fromList [toNasal a, toNasal b, toNasal c, toNasal d, toNasal e, toNasal f, toNasal g]
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
