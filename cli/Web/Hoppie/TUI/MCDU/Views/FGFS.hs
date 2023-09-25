{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS
where

import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.StringUtil
import Web.Hoppie.FGFS.Connection
import Web.Hoppie.FGFS.NasalValue

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.QQ (s)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.State
import Control.Monad
import Text.Printf
import Data.Text.Encoding
import Data.Maybe

data FPLeg =
  FPLeg
    { legName :: Text
    , legHeading :: Maybe Double
    , legDist :: Maybe Double
    , legRouteDist :: Maybe Double
    , legSpeed :: Maybe Double
    , legSpeedType :: Maybe Text
    , legAlt :: Maybe Double
    , legAltType :: Maybe Text
    , legParent :: Maybe Text
    , legRole :: Maybe Text
    , legIsDiscontinuity :: Bool
    }

instance FromNasal FPLeg where
  fromNasal n =
    FPLeg
      <$> fromNasalField "name" n
      <*> fromNasalField "heading" n
      <*> fromNasalField "leg_dist" n
      <*> fromNasalField "route_dist" n
      <*> fromNasalField "speed" n
      <*> fromNasalField "speed_type" n
      <*> fromNasalField "alt" n
      <*> fromNasalField "alt_type" n
      <*> fromNasalField "parent" n
      <*> fromNasalField "role" n
      <*> (fromMaybe False <$> fromNasalField "discontinuity" n)

formatDistance :: Double -> String
formatDistance dist
  | dist < 10
  = printf "%4.1fNM" dist
  | otherwise
  = printf "%4.0fNM" dist

formatAltitude :: Maybe Double -> Maybe Text -> String
formatAltitude (Just alt) (Just cstr) =
  altStr ++ conStr
  where
    altStr = case () of
      () | alt <= 18000
         -> printf "%5.0f" alt
      () | otherwise
         -> printf "FL%3.0f" (alt / 100)
    conStr = case cstr of
      "above" -> "A"
      "below" -> "B"
      "at" -> " "
      _ -> " "
formatAltitude _ _ = "  ---"

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

formatETA :: Double -> String
formatETA eta =
  let (minutesRaw :: Int) = floor eta `mod` (24 * 60)
      (hours, minutes) = minutesRaw `divMod` 60
  in printf "%02i%02i" hours minutes

fplView :: MCDUView
fplView = defView
  { mcduViewTitle = "ACT RTE LEGS"
  , mcduViewAutoReload = True
  , mcduViewOnLoad = do
      connMay <- gets mcduFlightgearConnection
      case connMay of
        Nothing -> do
          modifyView $ \v -> v
            { mcduViewNumPages = 1
            , mcduViewPage = 0
            , mcduViewDraw = do
                mcduPrintC (screenW `div` 2) (screenH `div` 2) red "FGFS NOT CONNECTED"
            , mcduViewLSKBindings = mempty
            }
        Just conn -> do
          (groundspeed :: Double) <- fmap (max 100) . liftIO $ runNasal conn [s| getprop('/velocities/groundspeed-kt') |]
          (utcMinutes :: Double) <- liftIO $ runNasal conn
            [s| var hour = getprop('/sim/time/utc/hour');
                var minute = getprop('/sim/time/utc/minute');
                var second = getprop('/sim/time/utc/second');
                return (hour * 60 + minute + second / 60);
              |]
          legs' <- liftIO $ runNasal conn
            [s| var fp = flightplan();
                var result = [];
                for (var i = 0; i < fp.getPlanSize(); i += 1) {
                  var wp = fp.getWP(i);
                  var parent_id = nil;
                  if (wp.wp_parent != nil)
                    parent_id = wp.wp_parent.id;
                  append(result,
                    { "name": wp.wp_name
                    , "heading": wp.leg_bearing
                    , "leg_dist": wp.leg_distance
                    , "route_dist": wp.distance_along_route
                    , "speed": wp.speed_cstr
                    , "speed_type": wp.speed_cstr_type
                    , "alt": wp.alt_cstr
                    , "alt_type": wp.alt_cstr_type
                    , "parent": parent_id
                    , "role": wp.wp_role
                    , "discontinuity": (wp.wp_type == "discontinuity" or wp.wp_type == "vectors")
                    });
                }
                return result;
              |]
          currentLeg <- liftIO $ runNasal conn
            [s| var fp = flightplan();
                return fp.current
              |]
          let legs = case currentLeg of
                        Nothing -> legs'
                        Just i -> drop i legs'
          curPage <- gets (mcduViewPage . mcduView)
          let legsDropped = curPage * 5
          let numPages = (length legs + 4) `div` 5
          let curLegs = take 5 . drop legsDropped $ legs
          modifyView $ \v -> v
            { mcduViewNumPages = numPages
            , mcduViewDraw = do
                zipWithM_
                  (\n leg -> do
                    let isCurrent = (n + legsDropped == 0)
                        color = if isCurrent then
                                  magenta
                                else if legIsDiscontinuity leg then
                                  white
                                else if (legRole leg == Just "missed") then
                                  cyan
                                else
                                  green
                    forM_ (legRouteDist leg) $ \routeDist -> do
                      when (groundspeed > 40) $ do
                        let eta = utcMinutes + routeDist / groundspeed * 60
                        mcduPrint (screenW - 6) (n * 2 + 1) color (BS8.pack $ formatETA eta <> "z")
                    mcduPrint 1 (n * 2 + 1) color (BS8.pack . maybe "---°" (printf "%03.0f°") $ legHeading leg)
                    mcduPrint 9 (n * 2 + 1) color (BS8.pack . maybe "----NM" formatDistance $ legDist leg)
                    mcduPrint 0 (n * 2 + 2) color (encodeUtf8 $ legName leg)
                    mcduPrint (screenW - 11) (n * 2 + 2) color (BS8.pack $ formatSpeed (legSpeed leg) (legSpeedType leg))
                    mcduPrint (screenW - 7) (n * 2 + 2) color "/"
                    mcduPrint (screenW - 6) (n * 2 + 2) color (BS8.pack $ formatAltitude (legAlt leg) (legAltType leg))
                  ) [0,1..] curLegs
            }
  }
