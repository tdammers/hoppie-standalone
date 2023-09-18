{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Hoppie.TUI.MCDU.Draw
where

import Web.Hoppie.TUI.Output
import Web.Hoppie.TUI.Input

import Control.Monad
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.Word
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import System.IO
import Control.Monad.Reader
import Control.Monad.ST


screenW :: Int
screenW = 24

screenH :: Int
screenH = 12

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
    { mcduScreenLines :: Vector MCDUCell
    }

emptyMCDUScreenBuffer :: MCDUScreenBuffer
emptyMCDUScreenBuffer =
  MCDUScreenBuffer $
    Vector.replicate (screenW * screenH) defCell

type MCDUDraw s = ReaderT (MVector s MCDUCell) (ST s)

runMCDUDraw :: (forall s. MCDUDraw s ()) -> MCDUScreenBuffer -> MCDUScreenBuffer
runMCDUDraw action =
  MCDUScreenBuffer . Vector.modify (runReaderT action) . mcduScreenLines

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

mcduPrintLskL :: Int -> ByteString -> MCDUDraw s ()
mcduPrintLskL n "" =
  mcduPrint 0 (mcduLskY n) white "<"
mcduPrintLskL n msg =
  mcduPrint 0 (mcduLskY n) white ("<" <> msg)

mcduPrintLskR :: Int -> ByteString -> MCDUDraw s ()
mcduPrintLskR n "" =
  mcduPrintR screenW (mcduLskY n) white ">"
mcduPrintLskR n msg =
  mcduPrintR screenW (mcduLskY n) white (msg <> ">")

mcduLskSY :: Int -> Int
mcduLskSY = screenY . mcduLskY

mcduLskY :: Int -> Int
mcduLskY 0 = 2
mcduLskY 1 = 4
mcduLskY 2 = 6
mcduLskY 3 = 8
mcduLskY 4 = 10
mcduLskY n = mcduLskY (n `rem` 5)

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
  putStr (centerTo 6 label)

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

  moveTo (screenW + 8) 3
  putStr " F6  "
  moveTo (screenW + 8) 5
  putStr " F7  "
  moveTo (screenW + 8) 7
  putStr " F8  "
  moveTo (screenW + 8) 9
  putStr " F9  "
  moveTo (screenW + 8) 11
  putStr " F10 "

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

  drawKey 1 (screenH + 1) "PgUp" "PREV"
  drawKey 1 (screenH + 3) "PgDn" "NEXT"
  drawKey 8 (screenH + 1) "Esc" "MENU"
  drawKey 15 (screenH + 1) "F11" "DLK"
  drawKey 22 (screenH + 1) "F12" "ATC"

  resetFG
  resetBG

  redrawMCDU screenBuf
