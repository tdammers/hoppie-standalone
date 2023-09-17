{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.Hoppie.TUI.StringUtil
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.String
import Data.List

class StrLength a where
  strLength :: a -> Int

class WordSplit a where
  wordSplit :: a -> [a]
  wordJoin :: [a] -> a

class LineSplit a where
  lineSplit :: a -> [a]
  lineJoin :: [a] -> a

class Substring a where
  takeSubstr :: Int -> a -> a
  dropSubstr :: Int -> a -> a

instance StrLength ByteString where
  strLength = BS8.length

instance WordSplit ByteString where
  wordSplit = BS8.words
  wordJoin = BS8.unwords

instance LineSplit ByteString where
  lineSplit = BS8.lines
  lineJoin = BS8.unlines

instance Substring ByteString where
  takeSubstr = BS.take
  dropSubstr = BS.drop

instance StrLength [a] where
  strLength = length

instance WordSplit [Char] where
  wordSplit = words
  wordJoin = unwords

instance LineSplit [Char] where
  lineSplit = lines
  lineJoin = unlines

instance Substring [a] where
  takeSubstr = take
  dropSubstr = drop

lineWrap :: forall a.
            (StrLength a, WordSplit a, LineSplit a, Substring a)
         => Int -> a -> [a]
lineWrap w src =
  concat $ map (map wordJoin . go . wordSplit) $ lineSplit src
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = case breakWrappedLine w xs of
      (x, []) -> [x]
      (x, xs') ->
        x : go xs'

breakWrappedLine :: (StrLength a, WordSplit a, Substring a)
                 => Int -> [a] -> ([a], [a])
breakWrappedLine _ [] = ([], [])
breakWrappedLine w (x:xs)
  | w <= 0
  = ([], x:xs)
  | strLength x > w
  = ([takeSubstr w x], dropSubstr w x : xs)
  | otherwise
  = let (x', xs') = breakWrappedLineCont (w - strLength x - 1) xs
    in (x : x', xs')

breakWrappedLineCont :: (StrLength a, WordSplit a, Substring a)
                     => Int -> [a] -> ([a], [a])
breakWrappedLineCont _ [] = ([], [])
breakWrappedLineCont w (x:xs)
  | strLength x > w
  = ([], x:xs)
  | otherwise
  = let (x', xs') = breakWrappedLineCont (w - strLength x - 1) xs
    in (x : x', xs')


newtype ColoredBS =
  ColoredBS { coloredBSFragments :: [ColoredBSFragment] }
  deriving newtype (Eq, Ord, Monoid, Semigroup)

data ColoredBSFragment =
  ColoredBSFragment
    { cbfColor :: !Word8
    , cbfData :: !ByteString
    }
    deriving (Eq, Ord)

instance IsString ColoredBS where
  fromString str = ColoredBS [ColoredBSFragment 15 $ BS8.pack str]

coloredTake :: Int -> ColoredBS -> ColoredBS
coloredTake _ (ColoredBS [])
  = ColoredBS []
coloredTake 0 (ColoredBS {})
  = ColoredBS []
coloredTake n (ColoredBS (x:xs))
  | BS.length (cbfData x) > n
  = ColoredBS [ColoredBSFragment (cbfColor x) (BS.take n (cbfData x))]
  | otherwise
  = let ColoredBS xs' = coloredTake (n - BS.length (cbfData x)) (ColoredBS xs)
    in ColoredBS (x : xs')

coloredDrop :: Int -> ColoredBS -> ColoredBS
coloredDrop _ (ColoredBS [])
  = ColoredBS []
coloredDrop 0 x
  = x
coloredDrop n (ColoredBS (x:xs))
  | BS.length (cbfData x) <= n
  = ColoredBS [ColoredBSFragment (cbfColor x) (BS.drop n (cbfData x))]
  | otherwise
  = coloredDrop (n - BS.length (cbfData x)) (ColoredBS xs)

coloredLineSplit :: ColoredBS -> [ColoredBS]
coloredLineSplit (ColoredBS []) = []
coloredLineSplit (ColoredBS (x:xs)) =
  let xSplit = map (ColoredBSFragment (cbfColor x)) (lineSplit $ cbfData x)
      xsSplit = coloredLineSplit $ ColoredBS xs
      lefts = map (ColoredBS . (:[])) $ init xSplit
      leftLast = ColoredBS $ takeEnd 1 xSplit
  in case xsSplit of
    [] -> lefts ++ [leftLast]
    (x':xs') -> lefts ++ [leftLast <> x'] ++ xs'

coloredWordSplit :: ColoredBS -> [ColoredBS]
coloredWordSplit (ColoredBS []) = []
coloredWordSplit (ColoredBS (x:xs)) =
  let xSplit = map (ColoredBSFragment (cbfColor x)) (wordSplit $ cbfData x)
      xsSplit = coloredWordSplit $ ColoredBS xs
      lefts = map (ColoredBS . (:[])) $ init xSplit
      leftLast = ColoredBS $ takeEnd 1 xSplit
  in case xsSplit of
    [] -> lefts ++ [leftLast]
    (x':xs') -> lefts ++ [leftLast <> x'] ++ xs'

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs

coloredLength :: ColoredBS -> Int
coloredLength (ColoredBS fragments) =
  sum $ map (BS.length . cbfData) fragments

instance StrLength ColoredBS where
  strLength = coloredLength

instance WordSplit ColoredBS where
  wordSplit = coloredWordSplit
  wordJoin = mconcat . intersperse (ColoredBS [ColoredBSFragment 15 " "])

instance LineSplit ColoredBS where
  lineSplit = coloredLineSplit
  lineJoin = mconcat . intersperse (ColoredBS [ColoredBSFragment 15 "\n"])

instance Substring ColoredBS where
  takeSubstr = coloredTake
  dropSubstr = coloredDrop

