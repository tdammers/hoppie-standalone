{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hoppie.TUI.MCDU.Views.Common
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.Views.Enum
import Web.Hoppie.TUI.StringUtil

import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word
import Text.Printf

{-# ANN module ("HLint: ignore redundant <$>" :: String) #-}

loadUplinkLSK :: LSK -> MCDU ()
loadUplinkLSK lsk = do
  unreadDLK <- gets mcduUnreadDLK
  unreadCPDLC <- gets mcduUnreadCPDLC
  case unreadCPDLC of
    Just cpdlcUID ->
      addLskBinding lsk "ATC UPLINK" $
        loadViewByID (MessageView cpdlcUID)
    Nothing ->
      case unreadDLK of
        Just dlkUID ->
          addLskBinding lsk "DLK UPLINK" $
            loadViewByID (MessageView dlkUID)
        Nothing ->
          removeLskBinding lsk

paginateWithHeadroom :: Int
                     -> Int
                     -> Int
                     -> [a]
                     -> (Int, [a])
paginateWithHeadroom headroom itemsPerPage page items =
  let pageItems = if page < 0 then
                    []
                  else
                    take itemsPerPage . drop (itemsPerPage * page) $ items
      numPages = (length items + headroom + itemsPerPage - 1) `div` itemsPerPage
  in (numPages, pageItems)

paginate :: Int
         -> Int
         -> [a]
         -> (Int, [a])
paginate = paginateWithHeadroom 0

formatETE :: Double -> String
formatETE minutesRaw =
  let (hours, minutes) = floor minutesRaw `divMod` 60 :: (Int, Int)
  in
    if hours >= 24 then
      "+++++"
    else
      printf "%02i+%02i" hours minutes

formatDistance :: Double -> String
formatDistance dist
  | dist < 10
  = printf "%4.1fNM" dist
  | otherwise
  = printf "%4.0fNM" dist

formatDistanceCompact :: Double -> String
formatDistanceCompact dist
  | dist < 10
  = printf "%4.1f" dist
  | otherwise
  = printf "%4.0f" dist

formatAltitude :: Double -> Maybe Double -> Maybe Text -> String
formatAltitude transAlt (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= transAlt
         -> printf "%5.0f" alt
      () | otherwise
         -> printf "FL%03.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatAltitude _ _ _ = "  ---"

formatAltitudeCompact :: Double -> Maybe Double -> Maybe Text -> String
formatAltitudeCompact transAlt (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= transAlt
         -> printf "%1.0f" alt
      () | otherwise
         -> printf "FL%1.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> ""
      _ -> ""
formatAltitudeCompact _ _ _ = "-"


formatSpeed :: Maybe Double -> Maybe Text -> String
formatSpeed (Just speed) (Just cstr) =
  speedStr ++ conStr
  where
    speedStr = printf "%3.0f" speed
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatSpeed _ _ = "---"

formatEFOB :: Word8 -> Double -> Double -> Double -> Maybe Double -> Colored ByteString
formatEFOB _ _ _ _ Nothing = ""
formatEFOB defcolor finres cont maxFOB (Just efob)
  | maxFOB >= 10000
  , efob >= 1000000
  = colorize color "+++.+"
  | maxFOB >= 10000
  , efob <= -1000000
  = colorize color "---.-"
  | maxFOB >= 10000
  = colorize color . BS8.pack $ printf "%5.1f" (efob / 1000)
  | efob >= 10000
  = colorize color "++++"
  | maxFOB >= 10000
  , efob <= -10000
  = colorize color "----"
  | otherwise
  = colorize color . BS8.pack $ printf "%5.0f" efob
  where
    color
      | efob <= finres
      = red
      | efob <= finres + cont
      = yellow
      | otherwise
      = defcolor

formatSpeedCompact :: Maybe Double -> Maybe Text -> String
formatSpeedCompact (Just speed) (Just cstr) =
  speedStr ++ conStr
  where
    speedStr = printf "%1.0f" speed
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> ""
      _ -> ""
formatSpeedCompact _ _ = "-"

formatETA :: Double -> String
formatETA eta =
  let (minutesRaw :: Int) = floor eta `mod` (24 * 60)
      (hours, minutes) = minutesRaw `divMod` 60
  in printf "%02i%02i" hours minutes

selectWith :: MCDU [ByteString]
           -> ByteString
           -> ByteString
           -> (ByteString -> MCDU Bool)
           -> ByteString
           -> MCDUView
           -> MCDU ()
selectWith getItems selectTitle warnMsg handleValue returnTitle returnView = do
  itemsMay <- getItems
  case itemsMay of
    [] -> do
      scratchWarn warnMsg
    items -> do
      let handleResult Nothing = loadView returnView
          handleResult (Just item) = do
            handleValue item >>= \case
              True ->
                loadView returnView
              False -> do
                return ()
      loadView (selectView ("SELECT " <> selectTitle) items returnTitle handleResult)

warnOrSucceed :: Maybe ByteString -> MCDU Bool
warnOrSucceed Nothing = return True
warnOrSucceed (Just e) = do
  scratchWarn e
  return False

selectView :: ByteString -> [ByteString] -> ByteString -> (Maybe ByteString -> MCDU ()) -> MCDUView
selectView title options returnLabel handleResult= defView
  { mcduViewTitle = title
  , mcduViewNumPages = (length options + (2 * numLSKs) - 1) `div` (2 * numLSKs)
  , mcduViewLSKBindings = mempty
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      let curOptions = take (2 * numLSKs) . drop (curPage * 2 * numLSKs) $ options
      modifyView $ \v -> v {
        mcduViewLSKBindings = Map.fromList
          [ (n, (colorize white option, handleResult (Just option)))
          | (n, option) <- zip [LSKL 0 .. LSKR 4] curOptions
          ]
      }
      addLskBinding (LSKL 5) (colorize white returnLabel) (handleResult Nothing)
  }

data SelectViewOptions =
  SelectViewOptions
    { selectViewSingleSided :: Bool
    , selectViewBreakLines :: Bool
    , selectViewUnloadAction :: MCDU ()
    }


selectViewWith :: SelectViewOptions
               -> ByteString
               -> [(a, Colored ByteString)]
               -> ByteString
               -> (Maybe a -> MCDU ())
               -> MCDUView
selectViewWith svo title options returnLabel handleResult = defView
  { mcduViewTitle = title
  , mcduViewNumPages =
      (length options + itemsPerPage - 1) `div` itemsPerPage
  , mcduViewLSKBindings = mempty
  , mcduViewOnUnload = selectViewUnloadAction svo
  , mcduViewOnLoad = do
      curPage <- gets (mcduViewPage . mcduView)
      let curOptions = take itemsPerPage . drop (curPage * itemsPerPage) $ options
      if (selectViewBreakLines svo) then
        modifyView $ \v -> v
          { mcduViewDraw = do
              zipWithM_ (\n (_, optionLabel) -> do
                    let y = n * 2 + 2
                        lns = take 2 $ (lineWrap (screenW - 2) optionLabel) ++ repeat ""
                    case lns of
                      [ln1, ln2] -> do
                        mcduPrint 0 y white "<"
                        mcduPrintColored 1 y ln1
                        mcduPrintColored 1 (y + 1) ln2
                      _ -> do
                        mcduPrint 0 y yellow "< WEIRDNESS"
                  ) [0..4] curOptions
          , mcduViewLSKBindings = Map.fromList
            [ (n, ("", handleResult (Just optionValue)))
            | (n, (optionValue, _)) <- zip lsks curOptions
            ]
          }
      else
        modifyView $ \v -> v {
          mcduViewLSKBindings = Map.fromList
            [ (n, (optionLabel, handleResult (Just optionValue)))
            | (n, (optionValue, optionLabel)) <- zip lsks curOptions
            ]
        }
      addLskBinding (LSKL 5) (colorize white returnLabel) (handleResult Nothing)
  }
  where
    itemsPerPage = case selectViewSingleSided svo of
      True -> numLSKs
      False -> 2 * numLSKs
    lsks = case selectViewSingleSided svo of
      True -> map LSKL [0 .. 4]
      False -> [LSKL 0 .. LSKR 4]

