module ParseMap
    ( parseMap, getHorizontalValueOfMark
    ) where

import Data.List.Split
import Data.List
import Text.Read
import Data.Maybe

breakOnSlash :: String -> [String]
breakOnSlash = splitOn "/"

numberToSpaces :: Int -> String
numberToSpaces 0 = ""
numberToSpaces x = ' ' : numberToSpaces (x - 1)

numberParser :: String -> String
numberParser [] = []
numberParser [x] = [x]
numberParser (x:y:xs)
  | isJust (readMaybe [x, y] :: Maybe Int) = numberToSpaces (fromJust (readMaybe [x, y] :: Maybe Int)) ++ numberParser xs
numberParser (x:xs)
  | isJust (readMaybe [x] :: Maybe Int) = numberToSpaces (fromJust (readMaybe [x] :: Maybe Int)) ++ numberParser xs
  | otherwise = x : numberParser xs

parseMap :: String -> [String]
parseMap x = breakOnSlash $ numberParser x

getHorizontalValueOfMark :: Char -> [String] -> Int
getHorizontalValueOfMark _ [[]] = error "can't retrieve horizontal value"
getHorizontalValueOfMark _ [] = error "can't retrieve horizontal value"
getHorizontalValueOfMark c (x:xs)
  | isJust(elemIndex c x) = 1
  | otherwise = 1 + getHorizontalValueOfMark c xs
