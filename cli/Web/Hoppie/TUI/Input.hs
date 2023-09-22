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
import System.Console.Terminfo as Terminfo
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

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
  | InputF13
  | InputF14
  | InputF15
  | InputF16
  | InputF17
  | InputF18
  | InputF19
  | InputF20
  | InputF21
  | InputF22
  | InputF23
  | InputF24
  | InputF25
  | InputF26
  | InputF27
  | InputF28
  | InputF29
  | InputF30
  | InputF31
  | InputF32
  | InputF33
  | InputF34
  | InputF35
  | InputF36
  | InputF37
  | InputF38
  | InputF39
  | InputF40
  | InputF41
  | InputF42
  | InputF43
  | InputF44
  | InputF45
  | InputF46
  | InputF47
  | InputF48
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

loadKeyCodes :: IO (Map [Word8] InputCommand)
loadKeyCodes = do
  t <- Terminfo.setupTermFromEnv
  let lookups =
        [ (InputF1, Terminfo.functionKey 1)
        , (InputF2, Terminfo.functionKey 2)
        , (InputF3, Terminfo.functionKey 3)
        , (InputF4, Terminfo.functionKey 4)
        , (InputF5, Terminfo.functionKey 5)
        , (InputF6, Terminfo.functionKey 6)
        , (InputF7, Terminfo.functionKey 7)
        , (InputF8, Terminfo.functionKey 8)
        , (InputF9, Terminfo.functionKey 9)
        , (InputF10, Terminfo.functionKey 10)
        , (InputF11, Terminfo.functionKey 11)
        , (InputF12, Terminfo.functionKey 12)
        , (InputF13, Terminfo.functionKey 13)
        , (InputF14, Terminfo.functionKey 14)
        , (InputF15, Terminfo.functionKey 15)
        , (InputF16, Terminfo.functionKey 16)
        , (InputF17, Terminfo.functionKey 17)
        , (InputF18, Terminfo.functionKey 18)
        , (InputF19, Terminfo.functionKey 19)
        , (InputF20, Terminfo.functionKey 20)
        , (InputF21, Terminfo.functionKey 21)
        , (InputF22, Terminfo.functionKey 22)
        , (InputF23, Terminfo.functionKey 23)
        , (InputF24, Terminfo.functionKey 24)
        , (InputF25, Terminfo.functionKey 25)
        , (InputF26, Terminfo.functionKey 26)
        , (InputF27, Terminfo.functionKey 27)
        , (InputF28, Terminfo.functionKey 28)
        , (InputF29, Terminfo.functionKey 29)
        , (InputF30, Terminfo.functionKey 30)
        , (InputF31, Terminfo.functionKey 31)
        , (InputF32, Terminfo.functionKey 32)
        , (InputF33, Terminfo.functionKey 33)
        , (InputF34, Terminfo.functionKey 34)
        , (InputF35, Terminfo.functionKey 35)
        , (InputF36, Terminfo.functionKey 36)
        , (InputF37, Terminfo.functionKey 37)
        , (InputF38, Terminfo.functionKey 38)
        , (InputF39, Terminfo.functionKey 39)
        , (InputF40, Terminfo.functionKey 40)
        , (InputF41, Terminfo.functionKey 41)
        , (InputF42, Terminfo.functionKey 42)
        , (InputF43, Terminfo.functionKey 43)
        , (InputF44, Terminfo.functionKey 44)
        , (InputF45, Terminfo.functionKey 45)
        , (InputF46, Terminfo.functionKey 46)
        , (InputF47, Terminfo.functionKey 47)
        , (InputF48, Terminfo.functionKey 48)
        , (InputLeft, Terminfo.keyLeft)
        , (InputRight, Terminfo.keyRight)
        , (InputUp, Terminfo.keyUp)
        , (InputDown, Terminfo.keyDown)
        , (InputBackspace, Terminfo.keyBackspace)
        , (InputDel, Terminfo.keyDeleteChar)
        , (InputHome, Terminfo.keyHome)
        , (InputEnd, Terminfo.keyEnd)
        , (InputPgUp, Terminfo.keyPageUp)
        , (InputPgDn, Terminfo.keyPageDown)
        ]
  return $ Map.fromList . mapMaybe (makeTerminfoEntry t) $ lookups
  where
    makeTerminfoEntry :: Terminfo.Terminal
                      -> (InputCommand, Terminfo.Capability String)
                      -> Maybe ([Word8], InputCommand)
    makeTerminfoEntry t (cmd, cap) = do
      capStr <- Terminfo.getCapability t cap
      let chars = map (fromIntegral . ord) capStr
      return (chars, cmd)

runInput :: TChan Word8 -> IO ()
runInput chan = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  withSigwinchHandler sendFF go
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

readCommand :: MonadIO m => Map [Word8] InputCommand -> TChan Word8 -> m InputCommand
readCommand kcl chan = do
  c <- liftIO . atomically $ readTChan chan
  case c of
    0x03 -> return InputTerminate
    0x08 -> return InputBackspace
    0x09 -> return InputTab
    0x0A -> return InputEnter
    0x0C -> return InputRedraw -- actually form feed (FF)...
    0x1B -> readEscapeSequence kcl chan
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

data AnsiCharacterClass
  = C0ControlChar
  | SpaceChar
  | IntermediateChar
  | ParameterChar
  | UppercaseChar
  | LowercaseChar
  | DeleteChar
  | C1ControlChar
  | G1DisplayableChar
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

cclass :: Word8 -> AnsiCharacterClass
cclass c | c < 0x20 = C0ControlChar
cclass c | c == 0x20 = SpaceChar
cclass c | c < 0x30 = IntermediateChar
cclass c | c < 0x40 = ParameterChar
cclass c | c < 0x60 = UppercaseChar
cclass c | c < 0x7F = UppercaseChar
cclass c | c == 0x7F = DeleteChar
cclass c | c < 0xA0 = C1ControlChar
cclass c | c == 0xA0 = SpaceChar
cclass c | c < 0xFF = G1DisplayableChar
cclass c | c == 0xFF = DeleteChar
cclass _ = DeleteChar

isAlphaChar :: Word8 -> Bool
isAlphaChar c = cclass c `elem` [UppercaseChar, LowercaseChar]

readEscapeSequence :: MonadIO m => Map [Word8] InputCommand -> TChan Word8 -> m InputCommand
readEscapeSequence kcl chan = do
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
        byte <- nextByte
        return $ fromMaybe (InputG3 [byte]) $ Map.lookup [27, 0x4f, byte] kcl
      0x50 -> do
        InputDCS <$> readUntilST nextByte
      0x5b -> do
        bytes <- doWhile (not . isAlphaChar) nextByte
        if bytes == [0x5b] then do
          moreBytes <- doWhile (not . isAlphaChar) nextByte
          return $ fromMaybe (InputCSI bytes) $ Map.lookup ([27, 0x5b] ++ bytes ++ moreBytes) kcl
        else
          return $ fromMaybe (InputCSI bytes) $ Map.lookup ([27, 0x5b] ++ bytes) kcl
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
