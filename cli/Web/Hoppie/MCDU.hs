{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.MCDU
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

mcduPutChar :: Int -> Int -> Word8 -> Word8 -> MCDUDraw s ()
mcduPutChar x y fg c =
  unless (y < 0 || y >= screenH || x < 0 || x >= screenW) $ do
    bufRaw <- ask
    MVector.write bufRaw (x + y * screenW) (MCDUCell fg c)

mcduPrint :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrint x y fg bs =
  unless (y < 0 || y >= screenH) $ do
    zipWithM_ (\x c -> mcduPutChar x y fg c) [x, x+1 ..] (BS.unpack bs)

mcduPrintR :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrintR x y fg bs = do
  let x' = x - BS.length bs
  mcduPrint x' y fg bs

mcduPrintC :: Int -> Int -> Word8 -> ByteString -> MCDUDraw s ()
mcduPrintC x y fg bs = do
  let x' = x - (BS.length bs `div` 2)
  mcduPrint x' y fg bs

redrawMCDU :: MCDUScreenBuffer -> IO ()
redrawMCDU buf = do
  setBG 0
  forM_ [0..screenH-1] $ \y -> do
    forM_ [0..screenW-1] $ \x -> do
      let cell = fromMaybe defCell $ mcduScreenLines buf !? (x + screenW * y)
      printCell (x + 7) (y + 1) cell
  moveTo 0 (screenH + 3)
  hFlush stdout
  where
    printCell x y cell = do
      moveTo x y
      setFG (cellFG cell)
      BS8.putStr . BS.singleton . cellChar $ cell

drawMCDU :: MCDUScreenBuffer -> IO ()
drawMCDU screenBuf = do
  clearScreen

  fillRect 7 0 0 (screenW + 13) (screenH + 1)

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

  moveTo 0 (screenH + 3)

  redrawMCDU screenBuf