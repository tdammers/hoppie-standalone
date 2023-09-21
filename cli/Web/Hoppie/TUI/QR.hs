{-# LANGUAGE OverloadedStrings #-}
module Web.Hoppie.TUI.QR
where

import Codec.QRCode (QRImage (..))
import qualified Codec.QRCode as QR
import Data.ByteString (ByteString)
import Data.Vector.Unboxed ( (!?) )
import Control.Monad
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe
import Data.Text (Text)

import Web.Hoppie.TUI.Output

formatQR :: ByteString -> [Text]
formatQR txt =
  let img = fromMaybe (error "QR code generation failed") $
              QR.encodeBinary
                (QR.defaultQRCodeOptions QR.H)
                txt
      size = qrImageSize img
  in
    flip map [-1 .. succ size `div` 2] $ \l ->
      "█" <>
      mconcat (flip map [0 .. pred size] $ \x ->
        let upper = fromMaybe False $ qrImageData img !? (l * 2 * size + x)
            lower = fromMaybe False $ qrImageData img !? ((l * 2 + 1) * size + x)
        in
          case (upper, lower) of
            (True, True) -> "\160"
            (False, True) -> "▀"
            (True, False) -> "▄"
            (False, False) -> "█"
      ) <>  "█"
