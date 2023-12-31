{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.TUI.MCDU.Draw
where

import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.MCDU.Keys
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.Telex

import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=))
import qualified Data.Aeson as JSON
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import Data.Word
import System.IO

screenW :: Int
screenW = 24

screenH :: Int
screenH = 14

red :: Word8
red = 9

green :: Word8
green = 10

yellow :: Word8
yellow = 11

blue :: Word8
blue = 12

magenta :: Word8
magenta = 13

cyan :: Word8
cyan = 14

white :: Word8
white = 15

data MCDUCell =
  MCDUCell
    { cellFG :: Word8
    , cellChar :: Word8
    }

defCell :: MCDUCell
defCell = MCDUCell 15 32

data MCDUScreenBuffer =
  MCDUScreenBuffer
    { mcduScreenLines :: !(Vector MCDUCell)
    }

instance ToJSON MCDUScreenBuffer where
  toJSON buf =
    JSON.object
      [ "data" .= toJSON (packLineData $ mcduScreenLines buf)
      ]

instance FromJSON MCDUScreenBuffer where
  parseJSON = JSON.withObject "ScreenBuffer" $ \obj -> do
    rawData <- obj .: "data"
    return $ MCDUScreenBuffer $ unpackLineData rawData

data MCDUScreenBufferUpdate =
  MCDUScreenBufferUpdate !Int !(Vector MCDUCell)

instance ToJSON MCDUScreenBufferUpdate where
  toJSON (MCDUScreenBufferUpdate lineNo lineData) =
    JSON.object
      [ "line" .= lineNo
      , "data" .= packLineData lineData
      ]

packLineData :: Vector MCDUCell -> Text
packLineData = Vector.foldl (<>) "" . Vector.map packCellData

unpackLineData :: Text -> Vector MCDUCell
unpackLineData txt =
  Vector.fromList $ go (Text.unpack txt)
  where
    go (cChr : gChr : xs) =
      let c = ((ord8 cChr - ord8 '0') .&. 7) + 8
          g = ord8 gChr
      in
        MCDUCell c g : go xs
    go _ = []

packCellData :: MCDUCell -> Text
packCellData (MCDUCell color glyph) =
  (Text.pack . show) (color .&. 7) <> (Text.singleton . chr8) glyph


instance FromJSON MCDUScreenBufferUpdate where
  parseJSON = JSON.withObject "ScreenBufferUpdate" $ \obj ->
    MCDUScreenBufferUpdate
      <$> obj .: "line"
      <*> (unpackLineData <$> (obj .: "data"))
        
emptyMCDUScreenBuffer :: MCDUScreenBuffer
emptyMCDUScreenBuffer =
  MCDUScreenBuffer $
    Vector.replicate (screenW * screenH) defCell

type MCDUDraw s = ReaderT (MVector s MCDUCell) (ST s)

runMCDUDraw :: (forall s. MCDUDraw s ()) -> MCDUScreenBuffer -> MCDUScreenBuffer
runMCDUDraw action buf =
  buf
    { mcduScreenLines =
        Vector.modify (runReaderT action) (mcduScreenLines buf)
    }

mcduClearScreen :: MCDUDraw s ()
mcduClearScreen = do
  bufRaw <- ask
  MVector.set bufRaw defCell

mcduPutChar :: Int -> Int -> Word8 -> Word8 -> MCDUDraw s ()
mcduPutChar x y fg c =
  unless (y < 0 || y >= screenH || x < 0 || x >= screenW) $ do
    bufRaw <- ask
    MVector.write bufRaw (x + y * screenW) (MCDUCell fg c)

mcduPrint :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrint x y fg bs =
  unless (y < 0 || y >= screenH) $ do
    zipWithM_ (\xx c -> mcduPutChar xx y fg c) [x, x+1 ..] (BS.unpack bs)

mcduPrintR :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrintR x y fg bs = do
  let x' = x - BS.length bs
  mcduPrint x' y fg bs

mcduPrintC :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrintC x y fg bs = do
  let x' = x - (BS.length bs `div` 2)
  mcduPrint x' y fg bs

mcduPrintColored :: Int -> Int -> Colored ByteString -> MCDUDraw s ()
mcduPrintColored _ _ (Colored []) =
  return ()
mcduPrintColored x y (Colored (f:fs)) = do
  mcduPrint x y (cbfColor f) bs
  mcduPrintColored (x + BS.length bs) y (Colored fs)
  where
    bs = cbfData f

mcduPrintColoredR :: Int -> Int -> Colored ByteString -> MCDUDraw s ()
mcduPrintColoredR _ _ (Colored []) =
  return ()
mcduPrintColoredR x y c = do
  mcduPrintColored (x - strLength c) y c

mcduPrintLskL :: Int -> Colored ByteString -> MCDUDraw s ()
mcduPrintLskL n "" =
  mcduPrintColored 0 (mcduLskY $ LSKL n) ""
mcduPrintLskL n msg =
  mcduPrintColored 0 (mcduLskY $ LSKL n) ("<" <> msg)

mcduPrintLskR :: Int -> Colored ByteString -> MCDUDraw s ()
mcduPrintLskR n "" =
  mcduPrintColoredR screenW (mcduLskY $ LSKR n) ""
mcduPrintLskR n msg =
  mcduPrintColoredR screenW (mcduLskY $ LSKR n) (msg <> ">")

mcduLskSY :: LSK -> Int
mcduLskSY = screenY . mcduLskY

mcduLskY :: LSK -> Int
mcduLskY (LSKL i) = 2 + 2 * i
mcduLskY (LSKR i) = 2 + 2 * i

screenX :: Int -> Int
screenX = (+ 7)

screenY :: Int -> Int
screenY = (+ 1)

redrawMCDULine :: Int -> MCDUScreenBuffer -> IO ()
redrawMCDULine y buf = do
  setBG 0
  forM_ [0..screenW-1] $ \x -> do
    let cell = fromMaybe defCell $ mcduScreenLines buf !? (x + screenW * y)
    printCell (screenX x) (screenY y) cell

printCell :: Int -> Int -> MCDUCell -> IO ()
printCell x y cell = do
  moveTo x y
  setFG (cellFG cell)
  BS8.putStr . BS.singleton . cellChar $ cell

redrawMCDU :: MCDUScreenBuffer -> IO ()
redrawMCDU buf = do
  setBG 0
  forM_ [0..screenH-1] $ \y -> do
    redrawMCDULine y buf
  moveTo 0 (screenH + 6)
  resetFG
  resetBG
  hFlush stdout

drawKey :: Int -> Int -> String -> String -> IO ()
drawKey x y key label = do
  moveTo x y
  setBG 7
  setFG 8
  putStr key

  moveTo x (y+1)
  setBG 0
  setFG 15
  putStr (centerTo 5 label)

centerTo :: Int -> String -> String
centerTo w xs
  | length xs == w
  = xs
  | length xs < w
  = let l = replicate ((w - length xs) `div` 2) ' '
        r = replicate (w - length xs - length l) ' '
    in l ++ xs ++ r
  | otherwise
  = take w xs

drawMCDU :: MCDUScreenBuffer -> IO ()
drawMCDU screenBuf = do
  resetFG
  resetBG

  clearScreen

  fillRect 7 0 0 (screenW + 13) (screenH + 5)

  fillRect 0 7 1 (screenW + 6) (screenH + 0)

  setBG 0
  setFG 15

  moveTo 1 3
  putStr "  F1 "
  moveTo 1 5
  putStr "  F2 "
  moveTo 1 7
  putStr "  F3 "
  moveTo 1 9
  putStr "  F4 "
  moveTo 1 11
  putStr "  F5 "
  moveTo 1 13
  putStr "  F6 "

  moveTo (screenW + 8) 3
  putStr " F7  "
  moveTo (screenW + 8) 5
  putStr " F8  "
  moveTo (screenW + 8) 7
  putStr " F9  "
  moveTo (screenW + 8) 9
  putStr " F10 "
  moveTo (screenW + 8) 11
  putStr " F11 "
  moveTo (screenW + 8) 13
  putStr " F12 "

  setBG 7
  setFG 0

  moveTo 6 3
  putStr "-"
  moveTo 6 5
  putStr "-"
  moveTo 6 7
  putStr "-"
  moveTo 6 9
  putStr "-"
  moveTo 6 11
  putStr "-"
  moveTo 6 13
  putStr "-"

  moveTo (screenW + 7) 3
  putStr "-"
  moveTo (screenW + 7) 5
  putStr "-"
  moveTo (screenW + 7) 7
  putStr "-"
  moveTo (screenW + 7) 9
  putStr "-"
  moveTo (screenW + 7) 11
  putStr "-"
  moveTo (screenW + 7) 13
  putStr "-"

  drawKey 1 (screenH + 1) "PgUp" "PREV"
  drawKey 7 (screenH + 1) "F13" "NAV"
  drawKey 13 (screenH + 1) "F14" "PROG"
  drawKey 19 (screenH + 1) "F15" "INIT"
  drawKey 25 (screenH + 1) "F16" "DLK"
  drawKey 31 (screenH + 1) "F17" "RADIO"

  drawKey 1 (screenH + 3) "PgDn" "NEXT"
  drawKey 7 (screenH + 3) "F18" "FPL"
  drawKey 13 (screenH + 3) "F19" "RTE"
  drawKey 19 (screenH + 3) "F20" ""
  drawKey 25 (screenH + 3) "F21" "ATC"
  drawKey 31 (screenH + 3) "Esc" "MENU"

  resetFG
  resetBG

  redrawMCDU screenBuf
