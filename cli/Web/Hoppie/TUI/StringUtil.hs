{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Hoppie.TUI.StringUtil
( StrLength (..)
, Substring (..)
, CharacterClass (..)
, PutStr (..)

, wordSplit
, wordJoin
, lineSplit
, lineJoin

, lineWrap

, Colored (..)
, ColoredFragment (..)
, coloredTake
, coloredDrop
, coloredSplitAt
, coloredFindIndex
, coloredLength
, coloredToStr
, colorize
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word
import Data.String
import qualified Data.List as List
import Data.Char

isWordSep8 :: Word8 -> Bool
isWordSep8 = (<= 32)

isNewline8 :: Word8 -> Bool
isNewline8 = (== fromIntegral (ord '\n'))

-- * Typeclasses

class CharacterClass c where
  isWordSep :: c -> Bool
  isLineSep :: c -> Bool

class StrLength a where
  strLength :: a -> Int

class Substring a where
  takeSubstr :: Int -> a -> a
  dropSubstr :: Int -> a -> a
  splitSubstr :: Int -> a -> (a, a)
  splitSubstr n str = (takeSubstr n str, dropSubstr n str)

class PutStr a where
  putString :: a -> IO ()
  putStringLine :: a -> IO ()

class FindIndex s where
  type Token s
  findIndex :: (Token s -> Bool) -> s -> Maybe Int

-- * 'ByteString' instances

instance CharacterClass Word8 where
  isWordSep = isWordSep8
  isLineSep = isNewline8

instance StrLength ByteString where
  strLength = BS8.length

instance Substring ByteString where
  takeSubstr = BS.take
  dropSubstr = BS.drop
  splitSubstr = BS.splitAt

instance PutStr ByteString where
  putString = BS.putStr
  putStringLine = BS8.putStrLn

instance FindIndex ByteString where
  type Token ByteString = Word8
  findIndex = BS.findIndex

-- * 'Text' instances

instance CharacterClass Char where
  isWordSep c = isSpace c && c /= '\160'
  isLineSep = (== '\n')

instance StrLength Text where
  strLength = Text.length

instance Substring Text where
  takeSubstr = Text.take
  dropSubstr = Text.drop
  splitSubstr = Text.splitAt

instance PutStr Text where
  putString = Text.putStr
  putStringLine = Text.putStrLn

instance FindIndex Text where
  type Token Text = Char
  findIndex = Text.findIndex

-- * 'String' / list instances

instance StrLength [a] where
  strLength = length

instance Substring [a] where
  takeSubstr = take
  dropSubstr = drop
  splitSubstr = splitAt

instance PutStr [Char] where
  putString = putStr
  putStringLine = putStrLn

instance Eq a => FindIndex [a] where
  type Token [a] = a
  findIndex = List.findIndex

takeWhileSubstr :: (Substring a, FindIndex a, Monoid a)
                => (Token a -> Bool)
                -> a
                -> a
takeWhileSubstr p str =
  let indexMay = findIndex (not . p) str
  in
    case indexMay of
      Nothing -> str
      Just index -> takeSubstr index str

dropWhileSubstr :: (Substring a, FindIndex a, Monoid a)
                => (Token a-> Bool)
                -> a
                -> a
dropWhileSubstr p str =
  let indexMay = findIndex (not . p) str
  in
    case indexMay of
      Nothing -> mempty
      Just index -> dropSubstr index str

wordSplit :: (StrLength a, Substring a, FindIndex a, CharacterClass c, c ~ Token a, Monoid a)
          => a -> [a]
wordSplit str
  | strLength str == 0
  = []
  | otherwise
  = let l = takeWhileSubstr (not . isWordSep) str
        m = dropWhileSubstr (not . isWordSep) str
        r = dropWhileSubstr isWordSep m
    in
      l : wordSplit r

lineSplit :: (StrLength a, Substring a, FindIndex a, CharacterClass c, c ~ Token a, Monoid a)
          => a -> [a]
lineSplit str
  | strLength str == 0
  = []
  | otherwise
  = let l = takeWhileSubstr (not . isLineSep) str
        m = dropWhileSubstr (not . isLineSep) str
        r = dropWhileSubstr isLineSep m
    in
      l : lineSplit r

wordJoin :: (IsString a, Monoid a) => [a] -> a
wordJoin = mconcat . List.intersperse " "

lineJoin :: (IsString a, Monoid a) => [a] -> a
lineJoin = mconcat . List.intersperse "\n"

lineWrap :: forall a c.
            (StrLength a, Substring a, FindIndex a, CharacterClass c, c ~ Token a, Monoid a, IsString a)
         => Int -> a -> [a]
lineWrap w src =
  concatMap (map wordJoin . go . wordSplit) $ lineSplit src
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = case breakWrappedLine w xs of
      (x, []) -> [x]
      (x, xs') ->
        x : go xs'

breakWrappedLine :: (StrLength a, Substring a)
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

breakWrappedLineCont :: (StrLength a, Substring a)
                     => Int -> [a] -> ([a], [a])
breakWrappedLineCont _ [] = ([], [])
breakWrappedLineCont w (x:xs)
  | strLength x > w
  = ([], x:xs)
  | otherwise
  = let (x', xs') = breakWrappedLineCont (w - strLength x - 1) xs
    in (x : x', xs')


newtype Colored a =
  Colored { coloredBSFragments :: [ColoredFragment a] }
  deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

data ColoredFragment a =
  ColoredFragment
    { cbfColor :: !Word8
    , cbfData :: !a
    }
    deriving (Show, Eq, Ord)

instance IsString a => IsString (Colored a) where
  fromString str = Colored [ColoredFragment 15 $ fromString str]

coloredTake :: (StrLength a, Substring a) => Int -> Colored a -> Colored a
coloredTake _ (Colored [])
  = Colored []
coloredTake 0 (Colored {})
  = Colored []
coloredTake n (Colored (x:xs))
  | strLength (cbfData x) > n
  = Colored [ColoredFragment (cbfColor x) (takeSubstr n (cbfData x))]
  | otherwise
  = let Colored xs' = coloredTake (n - strLength (cbfData x)) (Colored xs)
    in Colored (x : xs')

coloredDrop :: (StrLength a, Substring a) => Int -> Colored a -> Colored a
coloredDrop _ (Colored [])
  = Colored []
coloredDrop 0 x
  = x
coloredDrop n (Colored (x:xs))
  | strLength (cbfData x) > n
  = Colored (ColoredFragment (cbfColor x) (dropSubstr n (cbfData x)) : xs)
  | otherwise
  = coloredDrop (n - strLength (cbfData x)) (Colored xs)

coloredSplitAt :: (StrLength a, Substring a) => Int -> Colored a -> (Colored a, Colored a)
coloredSplitAt _ (Colored [])
  = (Colored [], Colored [])
coloredSplitAt 0 x
  = (Colored [], x)
coloredSplitAt n (Colored (x:xs))
  | strLength (cbfData x) > n
  = let (lhs, rhs) = splitSubstr n (cbfData x)
    in ( Colored [ColoredFragment (cbfColor x) lhs]
       , Colored (ColoredFragment (cbfColor x) rhs : xs)
       )
  | otherwise
  = let (Colored lhs, Colored rhs) = coloredSplitAt (n - strLength (cbfData x)) (Colored xs)
    in ( Colored (x : lhs)
       , Colored rhs
       )

instance (StrLength a, FindIndex a) => FindIndex (Colored a) where
  type Token (Colored a) = Token a
  findIndex = coloredFindIndex

coloredFindIndex :: (StrLength a, FindIndex a) => (Token (Colored a) -> Bool) -> Colored a -> Maybe Int
coloredFindIndex _ (Colored []) = Nothing
coloredFindIndex p (Colored (x:xs)) =
  case findIndex p (cbfData x) of
    Just n ->
      return n
    Nothing ->
      (strLength (cbfData x) +) <$> coloredFindIndex p (Colored xs)

coloredLength :: StrLength a => Colored a -> Int
coloredLength (Colored fragments) =
  sum $ map (strLength . cbfData) fragments

instance StrLength a => StrLength (Colored a) where
  strLength = coloredLength

instance (StrLength a, Substring a) => Substring (Colored a) where
  takeSubstr = coloredTake
  dropSubstr = coloredDrop
  splitSubstr = coloredSplitAt

coloredToStr :: Monoid a => Colored a -> a
coloredToStr (Colored fragments) = mconcat . map cbfData $ fragments

colorize :: Word8 -> a -> Colored a
colorize color str = Colored [ ColoredFragment color str ]
