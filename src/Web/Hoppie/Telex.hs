{-# LANGUAGE OverloadedStrings #-}
module Web.Hoppie.Telex
where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char

telexFilter :: ByteString -> ByteString
telexFilter = mconcat . map toTelex . BS.unpack

toTelex :: Word8 -> ByteString
toTelex c
  | isSpace8 c
  = BS.singleton $ ord8 ' '
  | c >= ord8 'a' && c <= ord8 'z'
  = BS.singleton $ c + ord8 'A' - ord8 'a'
  | c >= ord8 'A' && c <= ord8 'Z'
  = BS.singleton $ c
  | c `elem` telexFigures
  = BS.singleton $ c
  | otherwise
  = ""

telexFigures :: [Word8]
telexFigures = map ord8 "1234567890-'()+/:=?,.;\"'$#"

chr8 :: Word8 -> Char
chr8 = chr . fromIntegral

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

isSpace8 :: Word8 -> Bool
isSpace8 = isSpace . chr8
