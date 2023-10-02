module Web.Hoppie.StringUtil
where

import Data.Char

mapfirst :: (Char -> Char) -> String -> String
mapfirst _ [] = []
mapfirst f (x:xs) = f x : xs

ucfirst :: String -> String
ucfirst = mapfirst toUpper

lcfirst :: String -> String
lcfirst = mapfirst toLower
