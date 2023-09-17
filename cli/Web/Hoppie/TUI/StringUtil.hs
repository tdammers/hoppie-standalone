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

class StrLength a where
  strLength :: a -> Int

class WordSplit a where
  wordSplit :: a -> [a]
  wordJoin :: [a] -> a

class Substring a where
  takeSubstr :: Int -> a -> a
  dropSubstr :: Int -> a -> a

instance StrLength ByteString where
  strLength = BS8.length

instance WordSplit ByteString where
  wordSplit = BS8.words
  wordJoin = BS8.unwords

instance Substring ByteString where
  takeSubstr = BS.take
  dropSubstr = BS.drop

instance StrLength [a] where
  strLength = length

instance WordSplit [Char] where
  wordSplit = words
  wordJoin = unwords

instance Substring [a] where
  takeSubstr = take
  dropSubstr = drop

lineWrap :: forall a.
            (StrLength a, WordSplit a, Substring a)
         => Int -> a -> [a]
lineWrap w src =
  map wordJoin$ go (wordSplit src)
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


