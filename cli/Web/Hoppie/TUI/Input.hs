module Web.Hoppie.TUI.Input
where

import Web.Hoppie.TUI.Output

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import System.IO

data InputCommand
  = InputChar Char
  | InputEnter
  | InputTab
  | InputBackspace
  | InputIns
  | InputDel
  | InputHome
  | InputEnd
  | InputPgUp
  | InputPgDn
  | InputUp
  | InputDown
  | InputLeft
  | InputRight
  | InputShiftUp
  | InputShiftDown
  | InputShiftLeft
  | InputShiftRight
  | InputF1
  | InputF2
  | InputF3
  | InputF4
  | InputF5
  | InputF6
  | InputF7
  | InputF8
  | InputF9
  | InputF10
  | InputF11
  | InputF12
  | InputCtrlF1
  | InputCtrlF2
  | InputCtrlF3
  | InputCtrlF4
  | InputCtrlF5
  | InputCtrlF6
  | InputCtrlF7
  | InputCtrlF8
  | InputCtrlF9
  | InputCtrlF10
  | InputCtrlF11
  | InputCtrlF12
  | InputEscape
  | InputOtherC0 Word8
  | InputCSI [Word8]
  | InputG2 [Word8]
  | InputG3 [Word8]
  | InputDCS [Word8]
  | InputOSC [Word8]
  | InputUnknown [Word8]
  | InputRedraw
  | InputTerminate
  deriving (Show, Read, Eq, Ord)

runInput :: TChan Word8 -> IO ()
runInput chan = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  withSigwinchHandler sendFF $
    go
  where
    go = getByte >>= maybe (return ()) sendAndGo
    send = atomically . writeTChan chan
    sendAndGo c = send c >> go
    sendFF = atomically $ writeTChan chan 0x0C

getByte :: IO (Maybe Word8)
getByte = do
  bytes <- BS.unpack <$> BS.hGetSome stdin 1
  case bytes of
    [] -> return Nothing
    c:_ -> return (Just c)

readCommand :: MonadIO m => TChan Word8 -> m InputCommand
readCommand chan = do
  c <- liftIO . atomically $ readTChan chan
  case c of
    0x03 -> return InputTerminate
    0x08 -> return InputBackspace
    0x09 -> return InputTab
    0x0A -> return InputEnter
    0x0C -> return InputRedraw -- actually form feed (FF)...
    0x1B -> readEscapeSequence chan
    0x7F -> return InputBackspace
    _ | c < 0x20 -> return $ InputOtherC0 c
    _ | c < 0x80 -> return $ InputChar (chr $ fromIntegral c)
    _ -> readUtf8 c chan

readUtf8 :: MonadIO m => Word8 -> TChan Word8 -> m InputCommand
readUtf8 c0 chan = do
  let nextByte = liftIO . atomically $ readTChan chan
  if c0 .&. 0xf8 == 0xf0 then do
    -- 4-byte char
    c1 <- nextByte
    c2 <- nextByte
    c3 <- nextByte
    return . InputChar . chr $
      (fromIntegral (c0 .&. 0x0f) `shiftL` 18) .|.
      (fromIntegral c1 .&. 0x3f `shiftL` 12) .|.
      (fromIntegral c2 .&. 0x3f `shiftL` 6) .|.
      (fromIntegral c3 .&. 0x3f)
  else if c0 .&. 0xf0 == 0xe0 then do
    -- 3-byte char
    c1 <- nextByte
    c2 <- nextByte
    return . InputChar . chr $
      (fromIntegral (c0 .&. 0x0f) `shiftL` 12) .|.
      (fromIntegral c1 .&. 0x3f `shiftL` 6) .|.
      (fromIntegral c2 .&. 0x3f)
  else if c0 .&. 0xe0 == 0xc0 then do
    -- 2-byte char
    c1 <- nextByte
    return . InputChar . chr $
      (fromIntegral (c0 .&. 0x1f) `shiftL` 6) .|.
      (fromIntegral c1 .&. 0x3f)
  else
    error "UTF-8 error"

readEscapeSequence :: MonadIO m => TChan Word8 -> m InputCommand
readEscapeSequence chan = do
  let nextByteEither = liftIO $
        race
          (threadDelay 1000)
          (atomically (readTChan chan))
      nextByte = liftIO . atomically $ readTChan chan
  cEither <- nextByteEither
  case cEither of
    Left () -> return InputEscape
    Right c -> case c of
      0x4e -> do
        InputG2 <$> doWhile (>= 0x20) nextByte
      0x4f -> do
        InputG3 <$> doWhile (>= 0x20) nextByte
      0x50 -> do
        InputDCS <$> readUntilST nextByte
      0x5b -> do
        bytes <- doWhile (\x -> x < 0x40 || x > 0x7E) nextByte
        case bytes of
          [65] -> return InputUp
          [66] -> return InputDown
          [67] -> return InputRight
          [68] -> return InputLeft
          [97] -> return InputShiftUp
          [98] -> return InputShiftDown
          [99] -> return InputShiftRight
          [100] -> return InputShiftLeft

          [50,126] -> return InputIns
          [51,126] -> return InputDel
          [53,126] -> return InputPgUp
          [54,126] -> return InputPgDn
          [55,126] -> return InputHome
          [56,126] -> return InputEnd

          [49,49,126] -> return InputF1
          [49,50,126] -> return InputF2
          [49,51,126] -> return InputF3
          [49,52,126] -> return InputF4
          [49,53,126] -> return InputF5
          [49,55,126] -> return InputF6
          [49,56,126] -> return InputF7
          [49,57,126] -> return InputF8
          [50,48,126] -> return InputF9
          [50,49,126] -> return InputF10
          [50,51,126] -> return InputF11
          [50,52,126] -> return InputF12

          [49,49,94] -> return InputCtrlF1
          [49,50,94] -> return InputCtrlF2
          [49,51,94] -> return InputCtrlF3
          [49,52,94] -> return InputCtrlF4
          [49,53,94] -> return InputCtrlF5
          [49,55,94] -> return InputCtrlF6
          [49,56,94] -> return InputCtrlF7
          [49,57,94] -> return InputCtrlF8
          [50,48,94] -> return InputCtrlF9
          [50,49,94] -> return InputCtrlF10
          [50,51,94] -> return InputCtrlF11
          [50,52,94] -> return InputCtrlF12
          _ -> return $ InputCSI bytes
      0x5d -> do
        InputOSC <$> readUntilST nextByte
      _ -> do
        InputUnknown <$> readUntilST nextByte

readUntilST :: Monad m => m Word8 -> m [Word8]
readUntilST nextByte = do
  c <- nextByte
  if c == 0x5b then do
    c2 <- nextByte
    if c2 == 0x5c then do
      return []
    else do
      cc <- readUntilST nextByte
      return $ c : c2 : cc
  else do
    cc <- readUntilST nextByte
    return $ c : cc

doWhile :: Monad m => (a -> Bool) -> m a -> m [a]
doWhile cond action = do
  x <- action
  let t = cond x
  if t then
    (x:) <$> doWhile cond action
  else
    return [x]
