{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Hoppie.TUI.MCDU.Views.FGFS.PerfInit
where

import Web.Hoppie.FGFS.FMS as FMS
import Web.Hoppie.TUI.MCDU.Draw
import Web.Hoppie.TUI.MCDU.Monad
import Web.Hoppie.TUI.MCDU.Operations
import Web.Hoppie.TUI.MCDU.Views.Common
import Web.Hoppie.TUI.MCDU.Views.FGFS.Common
import Web.Hoppie.Telex

import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Printf
import Text.Read (readMaybe)

perfInitView :: MCDUView
perfInitView = defView
  { mcduViewTitle = "PERF INIT"
  , mcduViewAutoReload = False
  , mcduViewOnLoad = perfInitViewLoad
  , mcduViewNumPages = 4
  }

perfInitViewLoad :: MCDU ()
perfInitViewLoad = withFGView $ do
  pdVar <- liftIO . newIORef =<< getPerfInitData

  let doLoad = do
        massUnit <- gets mcduMassUnit
        let massFactor = case massUnit of
                            Kilograms -> 1
                            Pounds -> 1 / lbs2kg
            massUnitStr = case massUnit of
                            Kilograms -> "KG"
                            Pounds -> "LBS"
        let setMyCallsign (Just c) = do
              mcduSetCallsign c
              return True
            setMyCallsign Nothing =
              return False
            getMyCallsign =
              Just <$> mcduGetCallsign

        pd <- liftIO $ readIORef pdVar
        pdStored <- getPerfInitData
        currentFuel <- getFuelOnBoard
        aircraftModel <- getFGAircraftType

        let setZFW Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitZFW = Nothing }
              return True
            setZFW (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitZFW = Just (x / massFactor) }
                  return True
            getZFW =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitZFW pd

        let setBlockFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitBlockFuel = Nothing }
              return True
            setBlockFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitBlockFuel = Just (x / massFactor) }
                  return True
            getBlockFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitBlockFuel pd

        let setMinTakeoffFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitMinTakeoffFuel = Nothing }
              return True
            setMinTakeoffFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitMinTakeoffFuel = Just (x / massFactor) }
                  return True
            getMinTakeoffFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitMinTakeoffFuel pd

        let setContingencyFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitContingencyFuel = Nothing }
              return True
            setContingencyFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitContingencyFuel = Just (x / massFactor) }
                  return True
            getContingencyFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitContingencyFuel pd

        let setReserveFuel Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitReserveFuel = Nothing }
              return True
            setReserveFuel (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitReserveFuel = Just (x / massFactor) }
                  return True
            getReserveFuel =
              return $ BS8.pack . printf "%1.0f" . (* massFactor) <$> perfInitReserveFuel pd

        let setCruiseFL Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseFL = Nothing }
              return True
            setCruiseFL (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitCruiseFL = Just x, perfInitCruiseAlt = Nothing }
                  return True
            getCruiseFL =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseFL pd

        let setCruiseAlt Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseAlt = Nothing }
              return True
            setCruiseAlt (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitCruiseAlt = Just x, perfInitCruiseFL = Nothing }
                  return True
            getCruiseAlt =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseAlt pd

        let setCruiseMach Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseMach = Nothing }
              return True
            setCruiseMach (Just val) = do
              let val' = BS8.unpack val
                  val'' = case val' of
                            '.':_ -> '0' : val'
                            _ -> val'
              case readMaybe val'' of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  let mach = if x < 1.0 then x else x / 100
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseMach = Just mach
                      , perfInitCruiseIAS = Nothing
                      }
                  return True
            getCruiseMach =
              return $ BS8.pack . printf "%3.0f" . (* 100) <$> perfInitCruiseMach pd

        let setCruiseIAS Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseIAS = Nothing }
              return True
            setCruiseIAS (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseIAS = Just x
                      , perfInitCruiseMach = Nothing
                      }
                  return True
            getCruiseIAS =
              return $ BS8.pack . printf "%i" <$> perfInitCruiseIAS pd


        let setCruiseWind Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitCruiseWind = Nothing }
              return True
            setCruiseWind (Just val) = do
              let valStr = BS8.unpack val
                  (hdgStr, r) = span (/= '/') valStr
                  spdStr = drop 1 r
              case (readMaybe hdgStr, readMaybe spdStr) of
                (Just hdg, Just spd) ->  do
                  liftIO $ writeIORef pdVar $ pd
                      { perfInitCruiseWind = Just (hdg, spd)
                      }
                  return True
                _ -> do
                  scratchWarn "INVALID"
                  return False
            getCruiseWind =
              case perfInitCruiseWind pd of
                Nothing ->
                  return Nothing
                Just (hdg, spd) ->
                  return . Just $ BS8.pack $ printf "%03i/%02i" hdg spd

        let setTransAlt Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitTransAlt = Nothing }
              return True
            setTransAlt (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitTransAlt = Just x }
                  return True
            getTransAlt =
              return $ BS8.pack . printf "%i" <$> perfInitTransAlt pd

        let setTransFL Nothing = do
              liftIO $ writeIORef pdVar $ pd { perfInitTransFL = Nothing }
              return True
            setTransFL (Just val) = do
              case readMaybe . BS8.unpack $ val of
                Nothing -> do
                  scratchWarn "INVALID"
                  return False
                Just x ->  do
                  liftIO $ writeIORef pdVar $ pd { perfInitTransFL = Just x }
                  return True
            getTransFL =
              return $ BS8.pack . printf "%i" <$> perfInitTransFL pd

        let confirmInit = do
              setPerfInitData pd
              reloadView

            resetData = do
              getPerfInitData >>= liftIO . writeIORef pdVar
              reloadView

            isModified getter =
              let lhs = getter pd
                  rhs = getter pdStored
              in
                lhs /= rhs

            isModifiedF getter =
              let lhs = getter pd
                  rhs = getter pdStored
              in
                case (lhs, rhs) of
                  (Nothing, Nothing) -> False
                  (Just l, Just r) ->
                    abs (r - l) > 0.001
                  _ -> True

            fieldColorF getter =
              if isModifiedF getter then
                cyan
              else
                green

            fieldColor getter =
              if isModified getter then
                cyan
              else
                green

            modificationsExist =
              any isModifiedF
                [ perfInitZFW
                , perfInitBlockFuel
                , perfInitMinTakeoffFuel
                , perfInitContingencyFuel
                , perfInitReserveFuel
                , perfInitCruiseMach
                ] ||
              any isModified
                [ perfInitCruiseFL
                , perfInitCruiseAlt
                , perfInitCruiseIAS
                , perfInitTransAlt
                , perfInitTransFL
                ] ||
              isModified perfInitCruiseWind ||
              isModified perfInitClimbProfile

        actype <- gets mcduAircraftType
        callsign <- mcduGetCallsign
        let setACType :: Maybe ByteString -> MCDU Bool
            setACType actypeMay = do
              modify (\s -> s { mcduAircraftType = actypeMay })
              persistData
              return True
            getACType :: MCDU (Maybe ByteString)
            getACType =
              gets mcduAircraftType

        curPage <- gets (mcduViewPage . mcduView)
        let (numClimbPages, curClimbProfile) =
              paginateWithHeadroom 1 5 (curPage - 1) (perfInitClimbProfile pd)
        modifyView $ \v -> v
              { mcduViewNumPages = numClimbPages + 3 }
        case curPage of
          0 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT"
              , mcduViewDraw = do
                  mcduPrint 1 1 white "A/C TYPE"
                  mcduPrint 1 2 green (fromMaybe "----" actype)
                  mcduPrintR (screenW - 1) 1 white "ATC C/S"
                  mcduPrintR (screenW - 1) 2 green callsign
                  mcduPrint 1 3 white "A/C MODEL"
                  mcduPrint 1 4 green (fromMaybe "----" aircraftModel)

                  mcduPrint 1 5 white "UNITS"
                  mcduPrint 1 6 green (case massUnit of { Kilograms -> "KG"; Pounds -> "LBS" })
                  mcduPrintR (screenW - 1) 5 white "PERF MODE"
                  mcduPrintR (screenW - 1) 6 green "CUR FF/GS"

                  mcduPrint 1 7 white "TRANS ALT"
                  mcduPrintC (screenW `div` 2) 7 white "/"
                  mcduPrintR (screenW - 1) 7 white "TRANS LVL"
                  mcduPrint 1 8 (fieldColor perfInitTransAlt) $
                    maybe "-----" (BS8.pack . printf "%i") (perfInitTransAlt pd)
                  mcduPrintR (screenW - 1) 8 (fieldColor perfInitTransFL) $
                    maybe "-----" (BS8.pack . printf "FL%i") (perfInitTransFL pd)

              , mcduViewLSKBindings = Map.fromList $
                  [ (LSKL 0, ("", scratchInteract setACType getACType >> reloadView))
                  , (LSKR 0, ("", scratchInteract setMyCallsign getMyCallsign >> reloadView))
                  , (LSKL 2, ("", do
                               modify (\s -> s
                                 { mcduMassUnit = case massUnit of
                                     Kilograms -> Pounds
                                     Pounds -> Kilograms
                                 })
                               reloadView
                               persistData
                             )
                    )
                  , ( LSKL 3, ("", scratchInteract setTransAlt getTransAlt >> reloadView))
                  , ( LSKR 3, ("", scratchInteract setTransFL getTransFL >> reloadView))
                  ]
              }
          page | page >= 1 && page < numClimbPages + 1 -> do
            let climbPage = page - 1
            let profile = perfInitClimbProfile pd
                profileSize = length profile

            let formatIASOrMach (IAS i) = show i
                formatIASOrMach (Mach m) = printf "M%02.0f" (m * 100)
                color = if isModified perfInitClimbProfile then cyan else green

            let drawClimbPerf :: Int -> ClimbPerf -> MCDUDraw s ()
                drawClimbPerf n cp = do
                  mcduPrint 1 (2 * n + 2) white $ "CLB" <> BS8.pack (show $ n + 1)
                  mcduPrint 6 (2 * n + 2) color . BS8.pack $
                    printf "FL%03i / %3s / %4i"
                      (climbToFL cp)
                      (formatIASOrMach $ climbSpeed cp)
                      (climbRate cp)

            let setEntry :: Int -> Maybe ByteString -> MCDU Bool
                setEntry n' Nothing = do
                  let n = n' + climbPage * 5
                  if n < profileSize then do
                    liftIO $ writeIORef pdVar $ pd
                      { perfInitClimbProfile = take n profile ++ drop (n + 1) profile }
                    return True
                  else
                    return False
                setEntry n' (Just str) = do
                  let n = n' + climbPage * 5
                  case BS.split (ord8 '/') str of
                    [flStr, spdStr, rocStr] -> do
                      let parseResultMay = do
                            spd <- case BS.take 1 spdStr of
                                     "M" -> do
                                        mn <- readMaybe . BS8.unpack . BS.drop 1 $ spdStr
                                        Mach <$> if mn > 1.0 then
                                          return $ mn / 100
                                        else
                                          return mn

                                     _ -> IAS <$> (readMaybe . BS8.unpack $ spdStr)
                            roc <- readMaybe . BS8.unpack $ rocStr
                            fl <- readMaybe . BS8.unpack $ flStr
                            return $ ClimbPerf spd roc fl
                      case parseResultMay of
                        Nothing -> do
                          scratchWarn "INVALID"
                          return False
                        Just cp -> do
                          liftIO $ writeIORef pdVar $ pd
                            { perfInitClimbProfile = take n profile ++ [cp] ++ drop (n + 1) profile }
                          return True
                    _ -> do
                      scratchWarn "INVALID"
                      return False
                getEntry n' = do
                  let n = n' + climbPage * 5
                  case drop n $ profile of
                    [] -> return Nothing
                    (cp:_) -> return . Just . BS8.pack $
                      printf "%i/%s/%i"
                        (climbToFL cp)
                        (formatIASOrMach $ climbSpeed cp)
                        (climbRate cp)

            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT CLB"
              , mcduViewDraw = do
                  mcduPrint 6 1 white "TO FL / SPD / FPM"
                  zipWithM_ drawClimbPerf [0..5] curClimbProfile
                  let psn = profileSize - climbPage * 5
                  when (psn >= 0 && psn < 5) $
                          mcduPrint 6 (psn * 2 + 2) green "--- / ---- / ---"
                    
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKR n, ("", scratchInteract (setEntry n) (getEntry n) >> reloadView) )
                  | n' <- [0..profileSize]
                  , let n = n' + climbPage * 5
                  , n >= 0
                  , n <= 5
                  ]
              }
          page | page == numClimbPages + 1 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT CRZ"
              , mcduViewDraw = do
                  mcduPrint 1 1 white "CRZ MACH"
                  mcduPrintC (screenW `div` 2) 2 white "<-OR->"
                  mcduPrintR (screenW - 1) 1 white "CRZ IAS"
                  mcduPrint 1 2 (fieldColorF perfInitCruiseMach) $
                    maybe "---" (BS8.pack . printf "%3.2f") (perfInitCruiseMach pd)
                  mcduPrintR (screenW -1) 2 (fieldColor perfInitCruiseIAS) $
                    maybe "---" (BS8.pack . printf "%3i") (perfInitCruiseIAS pd)
                  mcduPrint 1 3 white "CRZ LVL"
                  mcduPrintC (screenW `div` 2) 4 white "<-OR->"
                  mcduPrintR (screenW - 1) 3 white "CRZ ALT"
                  mcduPrint 1 4 (fieldColor perfInitCruiseFL) $
                    maybe "---" (BS8.pack . printf "FL%i") (perfInitCruiseFL pd)
                  mcduPrintR (screenW - 1) 4 (fieldColor perfInitCruiseAlt) $
                    maybe "---" (BS8.pack . printf "%5i") (perfInitCruiseAlt pd)
                  mcduPrint 1 5 white "CRZ WIND"
                  mcduPrint 1 6 (fieldColor perfInitCruiseWind) $
                    maybe "---/--" (\(hdg, spd) -> BS8.pack $ printf "%03i/%02i" hdg spd) (perfInitCruiseWind pd)
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKL 0, ("", scratchInteract setCruiseMach getCruiseMach >> reloadView))
                  , ( LSKR 0, ("", scratchInteract setCruiseIAS getCruiseIAS >> reloadView))
                  , ( LSKL 1, ("", scratchInteract setCruiseFL getCruiseFL >> reloadView))
                  , ( LSKR 1, ("", scratchInteract setCruiseAlt getCruiseAlt >> reloadView))
                  , ( LSKL 2, ("", scratchInteract setCruiseWind getCruiseWind >> reloadView))
                  ]
              }
          page | page == numClimbPages + 2 -> do
            modifyView $ \v -> v
              { mcduViewTitle = "PERF INIT " <> massUnitStr
              , mcduViewDraw = do
                  let formatMass :: Maybe Double -> ByteString
                      formatMass = maybe "-----" (BS8.pack . printf "%7.0f" . (* massFactor))
                  mcduPrintR (screenW - 1) 1 white "ZFW"
                  mcduPrintR (screenW - 1) 2 (fieldColorF perfInitZFW) $ formatMass (perfInitZFW pd)
                  mcduPrint 1 3 white "GAUGE"
                  mcduPrint 1 4 white $ formatMass currentFuel
                  mcduPrintR (screenW - 1) 3 white "BLOCK FUEL"
                  mcduPrintR (screenW - 1) 4 (fieldColorF perfInitBlockFuel) $ formatMass (perfInitBlockFuel pd)
                  mcduPrintR (screenW - 1) 5 white "T/O FUEL"
                  mcduPrintR (screenW - 1) 6 (fieldColorF perfInitMinTakeoffFuel) $ formatMass (perfInitMinTakeoffFuel pd)
                  mcduPrintR (screenW - 1) 7 white "CONT FUEL"
                  mcduPrintR (screenW - 1) 8 (fieldColorF perfInitContingencyFuel) $ formatMass (perfInitContingencyFuel pd)
                  mcduPrintR (screenW - 1) 9 white "RSRV FUEL"
                  mcduPrintR (screenW - 1) 10 (fieldColorF perfInitReserveFuel) $ formatMass (perfInitReserveFuel pd)
              , mcduViewLSKBindings = Map.fromList $
                  [ ( LSKR 0, ("", scratchInteract setZFW getZFW >> reloadView))
                  , ( LSKR 1, ("", scratchInteract setBlockFuel getBlockFuel >> reloadView))
                  , ( LSKR 2, ("", scratchInteract setMinTakeoffFuel getMinTakeoffFuel >> reloadView))
                  , ( LSKR 3, ("", scratchInteract setContingencyFuel getContingencyFuel >> reloadView))
                  , ( LSKR 4, ("", scratchInteract setReserveFuel getReserveFuel >> reloadView))
                  ] ++
                  [ ( LSKL 5, ("RESET", resetData)) | modificationsExist ] ++
                  [ ( LSKR 5, ("CONFIRM INIT", confirmInit)) | modificationsExist ]
              }
          _ -> modifyView $ \v -> v
                { mcduViewDraw = do
                    mcduPrintC (screenW `div` 2) (screenH `div` 2) red "INVALID PAGE"
                , mcduViewLSKBindings = mempty
                }

  modifyView $ \v -> v
    { mcduViewOnLoad = doLoad
    , mcduViewAutoReload = True
    }
  doLoad

