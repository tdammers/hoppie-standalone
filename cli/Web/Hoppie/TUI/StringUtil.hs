{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.Hoppie.TUI.StringUtil
( StrLength (..)
, WordSplit (..)
, LineSplit (..)
, Substring (..)
, lineWrap

, ColoredBS (..)
, ColoredBSFragment (..)
, coloredTake
, coloredDrop
, coloredSplitAt
, coloredFindIndex
, coloredWordSplit
, coloredLineSplit
, coloredLength
, coloredToBS
, colorize
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.String
import Data.List
import Data.Char

isSpace8 :: Word8 -> Bool
isSpace8 = (<= 32)

isNewline8 :: Word8 -> Bool
isNewline8 = (== fromIntegral (ord '\n'))

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
  splitSubstr :: Int -> a -> (a, a)
  splitSubstr n str = (takeSubstr n str, dropSubstr n str)

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
  splitSubstr = BS.splitAt

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
  splitSubstr = splitAt

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
  deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

data ColoredBSFragment =
  ColoredBSFragment
    { cbfColor :: !Word8
    , cbfData :: !ByteString
    }
    deriving (Show, Eq, Ord)

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
  | BS.length (cbfData x) > n
  = ColoredBS (ColoredBSFragment (cbfColor x) (BS.drop n (cbfData x)) : xs)
  | otherwise
  = coloredDrop (n - BS.length (cbfData x)) (ColoredBS xs)

coloredSplitAt :: Int -> ColoredBS -> (ColoredBS, ColoredBS)
coloredSplitAt _ (ColoredBS [])
  = (ColoredBS [], ColoredBS [])
coloredSplitAt 0 x
  = (ColoredBS [], x)
coloredSplitAt n (ColoredBS (x:xs))
  | BS.length (cbfData x) > n
  = let (lhs, rhs) = BS.splitAt n (cbfData x)
    in ( ColoredBS [ColoredBSFragment (cbfColor x) lhs]
       , ColoredBS (ColoredBSFragment (cbfColor x) rhs : xs)
       )
  | otherwise
  = let (ColoredBS lhs, ColoredBS rhs) = coloredSplitAt (n - BS.length (cbfData x)) (ColoredBS xs)
    in ( ColoredBS (x : lhs)
       , ColoredBS rhs
       )

coloredFindIndex :: (Word8 -> Bool) -> ColoredBS -> Maybe Int
coloredFindIndex _ (ColoredBS []) = Nothing
coloredFindIndex p (ColoredBS (x:xs)) =
  case BS.findIndex p (cbfData x) of
    Just n ->
      return n
    Nothing ->
      (BS.length (cbfData x) +) <$> coloredFindIndex p (ColoredBS xs)

coloredWordSplit :: ColoredBS -> [ColoredBS]
coloredWordSplit (ColoredBS []) = []
coloredWordSplit xs
  = let i = coloredFindIndex (not . isSpace8) xs
        rest = maybe "" (flip coloredDrop xs) i
        j = coloredFindIndex (isSpace8) rest
        (lhs, rhs) = maybe (rest, mempty) (flip coloredSplitAt rest) j
    in
      if coloredLength lhs == 0 then
        coloredWordSplit rhs
      else
        lhs : coloredWordSplit rhs

coloredLineSplit :: ColoredBS -> [ColoredBS]
coloredLineSplit (ColoredBS []) = []
coloredLineSplit xs
  = let i = coloredFindIndex (not . isNewline8) xs
        rest = maybe "" (flip coloredDrop xs) i
        j = coloredFindIndex (isNewline8) rest
        (lhs, rhs) = maybe (rest, mempty) (flip coloredSplitAt rest) j
    in
      lhs : coloredLineSplit rhs

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
  splitSubstr = coloredSplitAt

coloredToBS :: ColoredBS -> ByteString
coloredToBS (ColoredBS fragments) = mconcat . map cbfData $ fragments

colorize :: Word8 -> ByteString -> ColoredBS
colorize color bs = ColoredBS [ ColoredBSFragment color bs ]
