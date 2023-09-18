{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Web.Hoppie.TUI.Output
where

import Web.Hoppie.TUI.TerminalSize

import Text.Printf
import Data.Word
import Control.Monad
import System.Posix.Signals ( Signal, sigINT, installHandler, Handler (..) )
import Control.Exception (bracket)
import Data.Bits
import Foreign.C

foreign import capi "signal.h value SIGWINCH"
  sigWINCH :: CInt

withSigwinchHandler :: IO () -> IO a -> IO a
withSigwinchHandler = withSignalHandler sigWINCH

withSigintHandler :: IO () -> IO a -> IO a
withSigintHandler = withSignalHandler sigINT

withSignalHandler :: Signal -> IO () -> IO a -> IO a
withSignalHandler sig onSignal action =
  bracket acquire release (const action)
  where
    acquire = installHandler sig handler Nothing
    release oldHandler = installHandler sig oldHandler Nothing
    handler = Catch onSignal
  
getScreenSize :: IO (Int, Int)
getScreenSize = getTermSize

moveTo :: PrintfType p => Int -> Int -> p
moveTo x y =
  printf "\27[%i;%iH" (y + 1) (x + 1)

clearScreen :: PrintfType p => p
clearScreen =
  printf "\27[2J"

setFG :: PrintfType p => Word8 -> p
setFG = printf "\27[38;5;%im"

setBG :: PrintfType p => Word8 -> p
setBG = printf "\27[48;5;%im"

resetFG :: PrintfType p => p
resetFG = printf "\27[39m"

resetBG :: PrintfType p => p
resetBG = printf "\27[49m"

resetAll :: PrintfType p => p
resetAll = printf "\27[0m"

setBold :: PrintfType p => p
setBold = printf "\27[1m"

setRegular :: PrintfType p => p
setRegular = printf "\27[22m"

clearRect :: Int -> Int -> Int -> Int -> IO ()
clearRect left top right bottom = do
  resetBG
  moveTo left top
  forM_ ([top .. bottom] :: [Int]) $ \y -> do
    moveTo left y
    putStr $ replicate (right - left + 1) ' '
  resetAll

fillRect :: Word8 -> Int -> Int -> Int -> Int -> IO ()
fillRect color left top right bottom = do
  setBG color
  moveTo left top
  forM_ ([top .. bottom] :: [Int]) $ \y -> do
    moveTo left y
    putStr $ replicate (right - left + 1) ' '
  resetAll
