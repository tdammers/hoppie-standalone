module Web.Hoppie.Telex
where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char

telexFilter :: ByteString -> ByteString
telexFilter = BS.map toTelex

toTelex :: Word8 -> Word8
toTelex c
  | isSpace8 c
  = ord8 ' '
  | c >= ord8 'a' && c <= ord8 'z'
  = c + ord8 'A' - ord8 'a'
  | c >= ord8 'A' && c <= ord8 'Z'
  = c
  | c `elem` telexFigures
  = c
  | otherwise
  = ord8 '?'

telexFigures :: [Word8]
telexFigures = map ord8 "1234567890-'()+/:=?,.;\"'$#"

chr8 :: Word8 -> Char
chr8 = chr . fromIntegral

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

isSpace8 :: Word8 -> Bool
isSpace8 = isSpace . chr8
